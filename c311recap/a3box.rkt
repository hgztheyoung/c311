#lang racket
;-----------------------A3------------------------
(println "-------------A3BOX-------------")

(define (empty-env)
  '())

(define (ext-env env k v)
  (cons `(,k . ,(box v)) env))

(define (lookup-update env k v)
  (let ([res (assv k env)])
    (if res
        (set-box! (cdr res) v)
        #f)))

(define (lookup env k)
  (let ([res (assv k env)])
    (if res
        (unbox (cdr res))
        #f)))

(struct Closure (nname body env))

(define (mkClosure name body env)
  (Closure name body env))

(define (value-of s)
  (define (literal? s)
    (or (boolean? s) (number? s)))
  (define (interp s env)
    (match s
      [l #:when (literal? l) l]
      [`(zero? ,b) (eq? (interp b env) 0)]
      [`(sub1 ,n) (- (interp n env) 1)]
      [`(* ,e1 ,e2) (* (interp e1 env) (interp e2 env))]
      [`(if ,pred ,conseq ,alt) (if (interp pred env)
                                    (interp conseq env)
                                    (interp alt env))]
      [v #:when (symbol? v) (lookup env v)]
      [`(lambda (,x) ,body) (mkClosure x body env)]
      [`(,rator ,rand)
       (let ([randv (interp rand env)]
             [ratorc (interp rator env)])
         (match ratorc
           [(Closure nname rbody renv) (interp rbody
                                               (ext-env renv nname randv)
                                               )]
           [x (print "expecting closure!!")]))]
      [`(let ([,name ,value]) ,body)
       (interp body (ext-env env name (interp value env)))]
      [`(set! ,k ,v) (lookup-update env k (interp v env))]
      [`(begin2 ,e1 ,e2) (begin (interp e1 env) (interp e2 env))]))
  (interp s (empty-env)))


test-equal?




;;tests

(value-of
 '((lambda (x) (if (zero? x)
                   12
                   47))
   0))
(value-of
 '(let ([y (* 3 4)])
    ((lambda (x) (* x y)) (sub1 6))))
(value-of
 '(let ([x (* 2 3)])
    (let ([y (sub1 x)])
      (* x y))))

(value-of
 '(let ([x (* 2 3)])
    (let ([x (sub1 x)])
      (* x x))))
(value-of
 '(let ((! (lambda (x) (* x x))))
    (let ((! (lambda (n)
               (if (zero? n) 1 (* n (! (sub1 n)))))))
      (! 5))))

(value-of
 '(((lambda (f)
      (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
    (lambda (f)
      (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
   5))


(value-of
 '((lambda (a)
     ((lambda (p)
        (begin2
          (p a)
          a)) (lambda (x) (set! x 4)))) 3))


(value-of
 '((lambda (f)
     ((lambda (g)
        ((lambda (z) (begin2
                       (g z)
                       z))
         55))
      (lambda (y) (f y)))) (lambda (x) (set! x 44))))

(value-of
 '((lambda (x)
     (begin2 (set! x 5) x))
   6))

(value-of
 '(let ((a 3))
    (begin2 (begin2 a (set! a 4)) a)))

(value-of
 '((lambda (x)
     (begin2
       ((lambda (y)
          (begin2
            (set! x 0)
            98))
        99)
       x))
   97))

(value-of
 '((lambda (y)
     (let ((x (begin2
                (set! y 7)
                8)))
       (begin2
         (set! y 3)
         ((lambda (z) y)
          x))))
   4))