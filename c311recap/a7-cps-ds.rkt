#lang racket
(require racket/pretty)
(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only 
            (error 'empty-k "You can only invoke the empty continuation once")
            (begin (set! once-only #t) v))))))

(define (empty-env)
  (lambda (x) (error 'empty-env "looking up empty env")))

(define (extend-env env k v)
  (lambda (n)
    (if (eq? n k)
        v
        (env n))))

(define (apply-env env k)
  (env k))

(define (apply-closure clo xv)
  (clo xv))


(define (empty-k-ds)
  `(empty-k))


  (define (mkClosure x body env)
    (lambda (xv)
      (ee body (extend-env env x xv) (empty-k-ds))))
  (define (zero?-inner k)
    `(zero?-inner ,k))
  (define (sub1-inner k)
    `(sub1-inner ,k))
  (define (*-inner v1 k)
    `(*-inner ,v1 ,k))
  (define (*-outer n2 env k)
    `(*-outer ,n2 ,env ,k))
  (define (if-outer conseq alt env k)
    `(if-outer ,conseq ,alt ,env ,k))  
  (define (begin2-outer e2 env k)
    `(begin2-outer ,e2 ,env ,k))
  (define (set!-outer env x k)
    `(set!-outer ,env ,x ,k))
  (define (random-outer k)
    `(random-outer ,k))  
  (define (ratorx-outer env x k)
    `(ratorx-outer ,env ,x ,k))
  (define (ratorrand-inner ratorv k)
    `(ratorrand-inner ,ratorv ,k))
  (define (ratorrand-outer rand env k)
    `(ratorrand-outer ,rand ,env ,k))
  (define (app-k k v)
    (match k
      [`(empty-k) v]
      [`(zero?-inner ,k) (app-k k (zero? v))]
      [`(sub1-inner ,k)
       (app-k k (sub1 v))]
      [`(*-inner ,v1 ,k)
       (app-k k (* v1 v))]
      [`(*-outer ,n2 ,env ,k)
       (ee n2 env (*-inner v k))]
      [`(if-outer ,conseq ,alt ,env ,k)
       (if v
           (ee conseq env k)
           (ee alt env k))]
      [`(begin2-outer ,e2 ,env ,k)
       (ee e2 env k)]
      [`(set!-outer ,env ,x ,k)    
       (app-k k (set-box! (apply-env env x) v))]
      [`(random-outer ,k)
       (app-k k (random v))]
      [`(ratorx-outer ,env ,x ,k)
       (app-k k (apply-closure v (box (unbox (apply-env env x)))))]
      [`(ratorrand-inner ,ratorv ,k)
       (app-k k (apply-closure ratorv (box v)))]
      [`(ratorrand-outer ,rand ,env ,k)
       (ee rand env (ratorrand-inner v k))]))

(define (ee exp env k)
  (match exp
    [b #:when (boolean? b) (app-k k b)]
    [n #:when (number? n) (app-k k n)]
    [`(zero? ,n) (ee n env (zero?-inner k))]
    [`(sub1 ,n) (ee n env (sub1-inner k))]
    [`(* ,n1 ,n2) (ee n1 env (*-outer n2 env k))]       
    [`(if ,test ,conseq ,alt)
     (ee test env (if-outer conseq alt env k))]
    [`(begin2 ,e1 ,e2) (ee e1 env (begin2-outer e2 env k))]
    [`(set! ,x ,v) #:when (symbol? x)                   
      (ee v env (set!-outer env x k))]
    [`(random ,n) (ee n env (random-outer k))]
    [x #:when (symbol? x) (app-k k (unbox (apply-env env x)))]
    [`(lambda (,x) ,body) (app-k k (mkClosure x body env))]
    [`(,rator ,x) #:when (symbol? x)
                  (ee rator env (ratorx-outer env x k))]
    [`(,rator ,rand) #:when (not (symbol? rand))
                     (ee rator env (ratorrand-outer rand env k))]))

;(ee `(* 1 2) (empty-env) (empty-k-ds))
;[`(* ,n1 ,n2) (ee n1 env (*-outer n2 env k))]       
;(ee 1 '() `(*-outer 2 '() `(empty-k)))
;(*-app-k `(*-outer 2 '() `(empty-k)) 1)
;(ee 2 `() `(*-inner 1 `(empty-k)))
;(*-app-k `(*-inner 1 `(empty-k)) 2)
;(*-app-k `(empty-k) (* 1 2))
;2

(ee `(* 1 2) (empty-env) (empty-k-ds))


(ee '(if (zero? (sub1 0)) 1 (begin2 1 (random (* 2 100)))) (empty-env) (empty-k-ds))


;--------------

(ee 'x (extend-env '() 'x (box 1)) (empty-k-ds))
(apply-closure (mkClosure 'x 'x `()) (box 1))
(app-k (empty-k-ds) (apply-closure (mkClosure 'x 'x `()) (box 1)))
(ee 1 `() (ratorrand-inner (mkClosure 'x 'x `()) (empty-k-ds)))
(app-k (ratorrand-outer 1 (empty-env) (empty-k-ds)) (mkClosure 'x 'x `()))
(ee `(lambda (x) x) (empty-env) (ratorrand-outer 1 (empty-env) (empty-k-ds)))
(ee `((lambda (x) x) 1) (empty-env) (empty-k-ds))

