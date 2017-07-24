#lang racket
(define id (lambda (x) x))

(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only 
            (error 'empty-k "You can only invoke the empty continuation once")
            (begin (set! once-only #t) v))))))



(define binary-to-decimal
  (lambda (n)
    (cond
      [(null? n) 0]
      [else (+ (car n) (* 2 (binary-to-decimal (cdr n))))])))

(define binary-to-decimal-cps
  (lambda (n k1)
    (cond
      [(null? n) (k1 0)]
      [else (binary-to-decimal-cps (cdr n) (lambda (b-dcdrn)
                  (k1 (+ (car n) (* 2 b-dcdrn)))))])))

(binary-to-decimal-cps `(1 0 1 1 1 1) id)

(define rember*1
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(pair? (car ls))
       (cond
         [(equal? (car ls) (rember*1 (car ls)))
          (cons (car ls) (rember*1 (cdr ls)))]
         [else (cons (rember*1 (car ls)) (cdr ls))])]
      [(eqv? (car ls) '?) (cdr ls)]
      [else (cons (car ls) (rember*1 (cdr ls)))])))


(define rember*1-cps
  (lambda (ls k1)
    (cond
      [(null? ls) (k1 '())]
      [(pair? (car ls))
       (cond
         [(rember*1-cps (car ls) (lambda (Rcarls)
              (equal? (car ls) Rcarls)))
          (rember*1-cps (cdr ls) (lambda (Rcdrls)
              (cons (car ls) Rcdrls)))]
         [else
          (rember*1-cps (car ls) (lambda (Rcarls)
               (cons Rcarls (cdr ls))))])]
      [(eqv? (car ls) '?) (k1 (cdr ls))]
      [else (rember*1-cps (cdr ls) (lambda (Rcdrls)
              (cons (car ls) Rcdrls)))])))


;;part 2

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

(define (val-of-cps exp env k)
  (define (mkClosure x body env)
    (lambda (xv)
      (val-of-cps body (extend-env env x xv) (empty-k))))
  (match exp
    [b #:when (boolean? b) (k b)]
    [n #:when (number? n) (k n)]
    [`(zero? ,n)  (val-of-cps n env (lambda (res)
                                      (k (zero? res))))]
    [`(sub1 ,n) (val-of-cps n env (lambda (res)
                                    (k (sub1 res))))]
    [`(* ,n1 ,n2) (val-of-cps n1 env (lambda (v1)                       
                       (val-of-cps n2 env (lambda (v2)
                           (k (* v1 v2))))))]
    [`(if ,test ,conseq ,alt)
     (val-of-cps test env (lambda (t)
          (if t
              (val-of-cps conseq env k)
              (val-of-cps alt env k))))]
    [`(begin2 ,e1 ,e2) (val-of-cps e1 env (lambda (e1res)
                          (val-of-cps e2 env k)))]
    [`(set! ,x ,v) #:when (symbol? x)                   
                   (val-of-cps v env (lambda (val)
                       (k (set-box! (apply-env env x) val))))]
    [`(random ,n) (val-of-cps n env (lambda (res)
                        (k (random res))))]
    [x #:when (symbol? x) (k (unbox (apply-env env x)))]
    [`(lambda (,x) ,body) (k (mkClosure x body env))]
    [`(,rator ,x) #:when (symbol? x)
                  (val-of-cps rator env (lambda (ratorv)
                       (k (ratorv (box (unbox (apply-env env x)))))))]
    [`(,rator ,rand) #:when (not (symbol? rand))
                     (val-of-cps rator env (lambda (ratorv)
                           (val-of-cps rand env (lambda (randv)
                               (k (apply-closure ratorv (box randv)))))))]))




(val-of-cps
 '((lambda (a)
     ((lambda (p)
        (begin2
          (p a)
          a)) (lambda (x) (set! x 4)))) 3)
 (empty-env) (empty-k))


(define (val-of-cps-fn exp env k)
  (define (mkClosure x body env)
    (lambda (xv)
      (val-of-cps-fn body (extend-env env x xv) (empty-k))))  
  (define (apply-closure clo xv)
    (clo xv))
  
  (define (app-k k v)
    (k v))
  (define (zero?-inner k)
    (lambda (res)
      (app-k k (zero? res))))
  (define (sub1-inner k)
    (lambda (res)
      (app-k k (sub1 res))))  
  (define (*-inner v1 k)
    (lambda (v2)
      (app-k k (* v1 v2))))
  (define (*-outer n2 env k)
    (lambda (v1)                       
      (val-of-cps-fn n2 env (*-inner v1 k))))
  (define (if-outer conseq alt env k)
    (lambda (t)
      (if t
          (val-of-cps-fn conseq env k)
          (val-of-cps-fn alt env k))))
  (define (begin2-outer e2 env k)
    (lambda (e1res)
      (val-of-cps-fn e2 env k)))
  (define (set!-outer env x k)
    (lambda (val)
      (app-k k (set-box! (apply-env env x) val))))
  (define (random-outer k)
    (lambda (res)
      (app-k k (random res))))
  (define (ratorx-outer env x k)
    (lambda (ratorv)
      (app-k k (apply-closure ratorv (box (unbox (apply-env env x)))))))
  (define (ratorrand-inner ratorv k)
    (lambda (randv)
      (app-k k (apply-closure ratorv (box randv)))))
  (define (ratorrand-outer rand env k)
    (lambda (ratorv)
      (val-of-cps-fn rand env (ratorrand-inner ratorv k))))
  (match exp
    [b #:when (boolean? b) (k b)]
    [n #:when (number? n) (k n)]
    [`(zero? ,n) (val-of-cps-fn n env (zero?-inner k))]
    [`(sub1 ,n) (val-of-cps-fn n env (sub1-inner k))]
    [`(* ,n1 ,n2) (val-of-cps-fn n1 env (*-outer n2 env k))]       
    [`(if ,test ,conseq ,alt)
     (val-of-cps-fn test env (if-outer conseq alt env k))]
    [`(begin2 ,e1 ,e2) (val-of-cps-fn e1 env (begin2-outer e2 env k))]
    [`(set! ,x ,v) #:when (symbol? x)                   
      (val-of-cps-fn v env (set!-outer env x k))]
    [`(random ,n) (val-of-cps-fn n env (random-outer k))]
    [x #:when (symbol? x) (app-k k (unbox (apply-env env x)))]
    [`(lambda (,x) ,body) (app-k k (mkClosure x body env))]
    [`(,rator ,x) #:when (symbol? x)
                  (val-of-cps-fn rator env (ratorx-outer env x k))]
    [`(,rator ,rand) #:when (not (symbol? rand))
                     (val-of-cps-fn rator env (ratorrand-outer rand env k))]))

(val-of-cps-fn
 '((lambda (a)
     ((lambda (p)
        (begin2
          (p a)
          a)) (lambda (x) (set! x 4)))) 3)
 (empty-env) (empty-k))

(define (empty-k-ds)
  `(empty-k))

(define (val-of-cps-ds exp env k)
  (define (mkClosure x body env)
    (lambda (xv)
      (val-of-cps-ds body (extend-env env x xv) (empty-k-ds))))
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
       (val-of-cps-ds n2 env (*-inner v k))]
      [`(if-outer ,conseq ,alt ,env ,k)
       (if v
           (val-of-cps-ds conseq env k)
           (val-of-cps-ds alt env k))]
      [`(begin2-outer ,e2 ,env ,k)
       (val-of-cps-ds e2 env k)]
      [`(set!-outer ,env ,x ,k)    
       (app-k k (set-box! (apply-env env x) v))]
      [`(random-outer ,k)
       (app-k k (random v))]
      [`(ratorx-outer ,env ,x ,k)
       (app-k k (apply-closure v (box (unbox (apply-env env x)))))]
      [`(ratorrand-inner ,ratorv ,k)
       (app-k k (apply-closure ratorv (box v)))]
      [`(ratorrand-outer ,rand ,env ,k)
       (val-of-cps-ds rand env (ratorrand-inner v k))]))
  (match exp
    [b #:when (boolean? b) (k b)]
    [n #:when (number? n) (app-k k n)]
    [`(zero? ,n) (val-of-cps-ds n env (zero?-inner k))]
    [`(sub1 ,n) (val-of-cps-ds n env (sub1-inner k))]
    [`(* ,n1 ,n2) (val-of-cps-ds n1 env (*-outer n2 env k))]       
    [`(if ,test ,conseq ,alt)
     (val-of-cps-ds test env (if-outer conseq alt env k))]
    [`(begin2 ,e1 ,e2) (val-of-cps-ds e1 env (begin2-outer e2 env k))]
    [`(set! ,x ,v) #:when (symbol? x)                   
      (val-of-cps-ds v env (set!-outer env x k))]
    [`(random ,n) (val-of-cps-ds n env (random-outer k))]
    [x #:when (symbol? x) (app-k k (unbox (apply-env env x)))]
    [`(lambda (,x) ,body) (app-k k (mkClosure x body env))]
    [`(,rator ,x) #:when (symbol? x)
                  (val-of-cps-ds rator env (ratorx-outer env x k))]
    [`(,rator ,rand) #:when (not (symbol? rand))
                     (val-of-cps-ds rator env (ratorrand-outer rand env k))]))

;(ee `(* 1 2) (empty-env) (empty-k-ds))
;[`(* ,n1 ,n2) (ee n1 env (*-outer n2 env k))]       
;(ee 1 '() `(*-outer 2 '() `(empty-k)))
;(*-app-k `(*-outer 2 '() `(empty-k)) 1)
;(ee 2 `() `(*-inner 1 `(empty-k)))
;(*-app-k `(*-inner 1 `(empty-k)) 2)
;(*-app-k `(empty-k) (* 1 2))
;2

(val-of-cps-ds `(* 1 2) (empty-env) (empty-k-ds))


(val-of-cps-ds '(if (zero? (sub1 0)) 1 (begin2 1 (random (* 2 100)))) (empty-env) (empty-k-ds))

(val-of-cps-ds 
 '((lambda (a)
     ((lambda (p)
        (begin2
          (p a)
          a)) (lambda (x) (set! x 4)))) 3) (empty-env) (empty-k-ds))


(define fact-5
  '((lambda (f)
      ((f f) 5))
    (lambda (f)
      (lambda (n)
        (if (zero? n)
            1
            (* n ((f f) (sub1 n))))))))
(define capture-fun
  '(* 3 (capture q (* 2 (return 4 q)))))
 (val-of-cps fact-5 (empty-env) (empty-k))
120
 (val-of-cps-fn fact-5 (empty-env) (empty-k))
120
 (val-of-cps-ds fact-5 (empty-env) (empty-k-ds))
120