#lang racket
;-----------------------A3------------------------
(println "-------------A3-------------")


(struct Closure (nname body env))

(define (mkClosure name body env)
  (Closure name body env))

(define (empty-env-fn)
  (lambda (x) (error 'empty-env-fn "looking up empty env")))

(define (ext-env-fn env k v)
  (lambda (n)
    (if (eq? n k)
        v
        (env n))))

(define (lookup-fn env k)
  (env k))

(define (empty-env)
  '())

(define (ext-env env k v)
  (cons `(,k . ,v) env))

(define (lookup env k)
  (let ([res (assv k env)])
    (if res
        (cdr res)
        #f)))

(set! empty-env empty-env-fn)
(set! ext-env ext-env-fn)
(set! lookup lookup-fn)

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
           [(Closure nname rbody renv) (interp rbody (ext-env renv nname randv))]
           [x (print "expecting closure!!")]))]
      [`(let ([,name ,value]) ,body)
       (interp body (ext-env env name (interp value env)))]
      ))
  (interp s (empty-env)))

(define cases
  (list
   '((lambda (x) (if (zero? x)
                     12
                     47))
     0)
   '(let ([y (* 3 4)])
      ((lambda (x) (* x y)) (sub1 6)))
   '(let ([x (* 2 3)])
      (let ([y (sub1 x)])
        (* x y)))

   '(let ([x (* 2 3)])
      (let ([x (sub1 x)])
        (* x x)))
   '(let ((! (lambda (x) (* x x))))
      (let ((! (lambda (n)
                 (if (zero? n) 1 (* n (! (sub1 n)))))))
        (! 5)))

   '(((lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
      (lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
     5)))

(map value-of cases)

#|
(define c0 (lambda (f) (lambda (x) x)))
(define c5 (lambda (f) (lambda (x) (f (f (f (f (f x))))))))
((c5 add1) 0)
((c0 add1) 0)

(define c+ (lambda (m)
             (lambda (n)
               (lambda (a) (lambda (b) ((m a) ((n a) b)))))))


(define csub1 (lambda (x) x))
(((csub1 c5) add1) 0)
;;church pred is hard
;; Turing took 3 months
|#
