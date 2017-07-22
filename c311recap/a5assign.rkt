#lang racket
(define (empty-env)
  (lambda (x) (error 'empty-env "looking up empty env")))

(define (extend-env env k v)
  (lambda (n)
    (if (eq? n k)
        v
        (env n))))

(define (apply-env env k)
  (env k))
  
(define (val-of-cbv exp env)
  (define (closure x body env)
    (lambda (xv)
      (val-of-cbv body (extend-env env x xv))))
  (define (apply-closure clo xv)
    (clo xv))

  (match exp
    [b #:when (boolean? b) b]
    [n #:when (number? n) n]
    [`(zero? ,n) (zero? (val-of-cbv n env))]
    [`(sub1 ,n) (sub1 (val-of-cbv n env))]
    [`(* ,n1 ,n2) (* (val-of-cbv n1 env)
                     (val-of-cbv n2 env))]
    [`(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                  (val-of-cbv conseq env)
                                  (val-of-cbv alt env))]

    [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
    [`(set! ,x ,v) #:when (symbol? x)
                   (set-box! (apply-env env x)
                             (val-of-cbv v env))]
    [`(random ,n) (random (val-of-cbv n env))]
    [x #:when (symbol? x) (unbox (apply-env env x))]
    [`(lambda (,x) ,body) (closure x body env)]
    ;;for call by value ,no need to specify this case
    ;;but it helpes to tell apart cbv and cbr
    [`(,rator ,x) #:when (symbol? x)
                  ((val-of-cbv rator env)
                   (box (unbox (apply-env env x))))]
    [`(,rator ,rand) #:when (not (symbol? rand))
                     (apply-closure (val-of-cbv rator env)
                                    (box (val-of-cbv rand env)))]))



(define (val-of-cbr exp env)
  (define (closure x body env)
    (lambda (xv)
      (val-of-cbr body (extend-env env x xv))))
  (define (apply-closure clo xv)
    (clo xv))

  (match exp
    [b #:when (boolean? b) b]
    [n #:when (number? n) n]
    [`(zero? ,n) (zero? (val-of-cbr n env))]
    [`(sub1 ,n) (sub1 (val-of-cbr n env))]
    [`(add1 ,n) (add1 (val-of-cbr n env))]
    [`(* ,n1 ,n2) (* (val-of-cbr n1 env)
                     (val-of-cbr n2 env))]
    [`(if ,test ,conseq ,alt) (if (val-of-cbr test env)
                                  (val-of-cbr conseq env)
                                  (val-of-cbr alt env))]

    [`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
    [`(set! ,x ,v) #:when (symbol? x)
                   (set-box! (apply-env env x)
                             (val-of-cbr v env))]
    [`(random ,n) (random (val-of-cbr n env))]
    [x #:when (symbol? x) (unbox (apply-env env x))]
    [`(lambda (,x) ,body) (closure x body env)]
    [`(,rator ,x) #:when (symbol? x)
                  ((val-of-cbr rator env)
                   (apply-env env x))]
    [`(,rator ,rand) #:when (not (symbol? rand))
                     (apply-closure (val-of-cbr rator env)
                                    (box (val-of-cbr rand env)))]))


(define (val-of-cbname exp env)
  (define (closure x body env)
    (lambda (xv)
      (val-of-cbname body (extend-env env x xv))))
  (define (apply-closure clo xv)
    (clo xv))

  (match exp
    [b #:when (boolean? b) b]
    [n #:when (number? n) n]
    [`(zero? ,n) (zero? (val-of-cbname n env))]
    [`(add1 ,n) (add1 (val-of-cbname n env))]
    [`(sub1 ,n) (sub1 (val-of-cbname n env))]
    [`(* ,n1 ,n2) (* (val-of-cbname n1 env)
                     (val-of-cbname n2 env))]
    [`(if ,test ,conseq ,alt) (if (val-of-cbname test env)
                                  (val-of-cbname conseq env)
                                  (val-of-cbname alt env))]
    [`(random ,n) (random (val-of-cbname n env))]
    [x #:when (symbol? x) ((unbox (apply-env env x)))]
    [`(lambda (,x) ,body) (closure x body env)]
    [`(,rator ,x) #:when (symbol? x)
                  ((val-of-cbname rator env)
                   (apply-env env x))]
    ;;passing the reference to the box 
    [`(,rator ,rand) #:when (not (symbol? rand))
                     (apply-closure (val-of-cbname rator env)
                                    (box (lambda () (val-of-cbname rand env))))]))



(define (val-of-cbneed exp env)
  (define (closure x body env)
    (lambda (xv)
      (val-of-cbneed body (extend-env env x xv))))
  (define (apply-closure clo xv)
    (clo xv))

  ;;modify the box to store the result and return the result.
  (define (unbox/need b)
    (let ([val ((unbox b))])
      (set-box! b (lambda () val))
      val))
  (match exp
    [b #:when (boolean? b) b]
    [n #:when (number? n) n]
    [`(zero? ,n) (zero? (val-of-cbneed n env))]
    [`(sub1 ,n) (sub1 (val-of-cbneed n env))]
    [`(* ,n1 ,n2) (* (val-of-cbneed n1 env)
                     (val-of-cbneed n2 env))]
    [`(if ,test ,conseq ,alt) (if (val-of-cbneed test env)
                                  (val-of-cbneed conseq env)
                                  (val-of-cbneed alt env))]
    [`(random ,n) (random (val-of-cbneed n env))]
    
    ;;evaluation happens until variable lookup
    [x #:when (symbol? x) (unbox/need (apply-env env x))]

    [`(lambda (,x) ,body) (closure x body env)]

    ;;passing the boxed x,which will be stored in the env and
    ;; be lookuped up and (unbox/evaluate/store result) in the future.
    [`(,rator ,x) #:when (symbol? x)
                  ((val-of-cbneed rator env)
                   (apply-env env x))]
    [`(,rator ,rand) #:when (not (symbol? rand))
                     (apply-closure (val-of-cbneed rator env)
                                    (box (lambda () (val-of-cbneed rand env))))]))





;;tests


  (val-of-cbv
 '((lambda (a)
     ((lambda (p)
        (begin2
          (p a)
          a)) (lambda (x) (set! x 4)))) 3)
 (empty-env))


(val-of-cbr
 '((lambda (a)
     ((lambda (p)
        (begin2
          (p a)
          a)) (lambda (x) (set! x 4)))) 3)
 (empty-env))

(define random-sieve
  '((lambda (n)
      (if (zero? n)
          (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n)
                                                                      (if (zero? n) #t #f) #f) #f) #f) #f) #f)
          (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if
                                                                               (zero? n) #f (if (zero? n) #f #t))))))))
    (random 2)))


(val-of-cbname random-sieve (empty-env))
(val-of-cbname
'((lambda (z) 100)
((lambda (x) (x x)) (lambda (x) (x x))))
(empty-env))
(val-of-cbneed random-sieve (empty-env))
