#lang racket


(define (empty-env)
  '())

(define (extend-env k v  env)
  (cons `(,k . ,v) env))

(define (apply-env env k)
  (let ([res (assv k env)])
    (if res
        (cdr res)
        #f)))

(define value-of-scopes-raw
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,x #:when (symbol? x) (apply-env env x)]
      [`(quote ()) '()]
      [`(null? ,ls) (null? (value-of-scopes-raw ls env))]
      [`(cons ,a ,d) (cons (value-of-scopes-raw a env) (value-of-scopes-raw d env))]
      [`(car ,ls) (car (value-of-scopes-raw ls env))]
      [`(cdr ,ls) (cdr (value-of-scopes-raw ls env))]
      [`(* ,nexp1 ,nexp2) (* (value-of-scopes-raw nexp1 env) (value-of-scopes-raw nexp2
                                                                          env))]
      [`(sub1 ,nexp) (sub1 (value-of-scopes-raw nexp env))]
      [`(if ,t ,c ,a) (if (value-of-scopes-raw t env)
                          (value-of-scopes-raw c env)
                          (value-of-scopes-raw a env))]
      [`(let ((,x ,e)) ,body) (let ((a (value-of-scopes-raw e env)))
                                (value-of-scopes-raw body (extend-env x a env)))]
      [`(lambda (,x) ,body) (lambda (a env^) (value-of-scopes-raw body (extend-env x a
                                                                               env)))]
      [`(d-lambda (,x) ,body) (lambda (a env^) (value-of-scopes-raw body (extend-env x a
                                                                                 env^)))]
      [`(,rator ,rand) ((value-of-scopes-raw rator env) (value-of-scopes-raw rand env)
                                                    env)])))

(define (empty-e)
  '())


(value-of-scopes-raw '(let ([x 5])
                    x)
                 (empty-env))

(value-of-scopes-raw '(let ([x 2])
                    (let ([f (d-lambda (e) x)])
                      (let ([x 5])
                        (f 0))))
                 (empty-env))




  (define (ext-env env k v)
    (cons `(,k . ,v) env))

  (define (lookup env k)
    (let ([res (assv k env)])
      (if res
          (cdr res)
          #f)))

(define (value-of-scopes s env)
  (define (literal? s)
    (or (boolean? s) (number? s)))
  ;scopeTag is 'd or 's
  (struct Closure  (nname body env scopeTag) #:transparent)

  (define (mkClosure name body env tag)
    (list 'Closure name body env tag))


  (define (interp s env)
    (match s
      [l #:when (literal? l) l]
      [`(zero? ,b) (eq? (interp b env) 0)]
      [`(sub1 ,n) (- (interp n env) 1)]
      [`(* ,e1 ,e2) (* (interp e1 env) (interp e2 env))]
      [`(if ,pred ,conseq ,alt) (if (interp pred env)
                                    (interp conseq env)
                                    (interp alt env))]
      [`(quote ()) '()]
      [`(null? ,ls) (null? (interp ls env))]
      [`(cons ,a ,d) (cons (interp a env) (interp d env))]
      [`(car ,ls) (car (interp ls env))]
      [`(cdr ,ls) (cdr (interp ls env))]

      
      [v #:when (symbol? v) (lookup env v)]
      [`(lambda (,x) ,body) (mkClosure x body env 's)]
      [`(d-lambda (,x) ,body) (mkClosure x body env 'd)]
      [`(,rator ,rand)
       (let ([randv (interp rand env)]
             [ratorc (interp rator env)])
         (match ratorc
           [(list 'Closure nname rbody renv tag)
            (cond
              [(eq? tag 's) (interp rbody (ext-env renv nname randv))]
              [(eq? tag 'd) (interp rbody (ext-env env nname randv))]
              [else (error "unknown tag")])]
           [x (println "expecting closure!!") (println x)]))]
      [`(let ([,name ,value]) ,body)
       (interp body (ext-env env name (interp value env)))]
      ))
  (interp s env))

(value-of-scopes '(let ([x 2])
                    (let ([f (d-lambda (e) x)])
                      (let ([x 5])
                        (f 0)))) (empty-e))
(value-of-scopes
 '(let
      ([l (cons 1 (cons 2 (cons 3 '())))])
    ((map (lambda (e) (cons e l))) l))
 (extend-env
  'map
  (value-of-scopes
   '(let ([map (lambda (map)
                 (lambda (f)
                   (lambda (l)
                     (if (null? l) '()
                         (cons (f (car l)) (((map map) f) (cdr l)))))))])
      (map map)) (empty-env))
  (empty-env)))

(value-of-scopes
 '(let
      ([l (cons 1 (cons 2 (cons 3 '())))])
    ((map (d-lambda (e) (cons e l))) l))
 (extend-env
  'map
  (value-of-scopes
   '(let ([map (lambda (map)
                 (lambda (f)
                   (lambda (l)
                     (if (null? l) '()
                         (cons (f (car l)) (((map map) f) (cdr l)))))))])
      (map map)) (empty-env))
  (empty-env)))

(value-of-scopes
 '(let
      ([map (d-lambda (f)
                      (d-lambda (l)
                                (if (null? l) '()
                                    (cons (f (car l)) ((map f) (cdr l))))))])
    (let ([f (d-lambda (e) (cons e l))])
      ((map f) (cons 1 (cons 2 (cons 3 '()))))))
 (empty-env))