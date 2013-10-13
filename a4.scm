;;kevin lu
;;a4.scm 
;; 9.21.2013
;; Worked with Steve

(load "test.scm") 
(load "pmatch.scm")

;; ===============================================
;;               PART I
;; ===============================================

;;functional representation of closure

(define value-of-fn
  (lambda (exp env)
    (pmatch-who "value-of-fn" exp
		[`,x (guard (symbol? x)) (apply-env-fn env x)]
		[`,n (guard (number? n)) n] 
		[`(if ,test ,conseq ,alt)
		 (if (value-of-fn test env) 
		     (value-of-fn conseq env) (value-of-fn alt env))]
		[`(lambda (,x) ,body)
	        (closure x body env)]
		
		;;original (lambda (a) (value-of body (ext-env x a env)))]
		;;(lambda (a env2) (value-of body (ext-env-fn x a env)))] 
		
		[`(zero? ,n-exp) (zero? (value-of-fn n-exp env))]
		;;[`(boolean? ,test1) (if (value-of-fn test1 env) #t #f)]
		[`,b (guard (boolean? b)) b]
		[`(* ,n-exp1 ,n-exp2)
		 (* (value-of-fn n-exp1 env) (value-of-fn n-exp2 env))]
		[`(sub1 ,n-exp) (sub1 (value-of-fn n-exp env))]
		;;LET CASE
		[`(let ((,x ,a)) ,body) 
		 (let ((t (value-of-fn a env)))
		   (value-of-fn body (ext-env-fn x t env)))]
		[`(,rator ,rand)
		 ;;(ext-env-fn x a env)])))
		 ;(apply-closure (value-of-fn rator env) (value-of-fn rand env))])))
		 (apply-closure (value-of-fn rator env) (value-of-fn rand env))])))

;(define empty-env-fn
;  (lambda ()
;   (lambda (y) (errorf 'empty env "unbound indentifier" y))))

(define ext-env-fn
  (lambda (x a env)
    (lambda (y) (if (eqv? x y) a (env y)))))

(define apply-env-fn
  (lambda (env x)
    (env x)))

(define empty-env
  (lambda ()
    '()))

;;box & unbox
(define box 
  (lambda (y)
    (cons 'box y)))

(define unbox 
  (lambda (b)
    (cdr b)))

(define set-box!
  (lambda (box val)
    (set-cdr! box val)))

;Functional representation with respect to closure
(define closure 
  (lambda (x body env)
    ;;`(closure ,x ,body ,env)))
    (lambda (a) (value-of-fn body (lambda (y) (if (eqv? x y) a (env y)))))))

(define apply-closure 
  (lambda (clos a)
    (clos a)))




;;scructural closure
(define value-of-ds
  (lambda (exp env)
    (pmatch-who "value-of-ds" exp
		[`,x (guard (symbol? x)) (apply-env-ds env x)]
		[`,n (guard (number? n)) n]
		[`(if ,test ,conseq ,alt)
		 (if (value-of-ds test env) 
		     (value-of-ds conseq env) (value-of-ds alt env))]
		[`(lambda (,x) ,body)
		 (closure-ds x body env)]
		 
		[`(d-lambda (,x) ,body) (lambda (a env^) (value-of-ds body (ext-env-ds x a env)))]

		;;Origin (lambda (a) (value-of-ds body (ext-env-ds x a env)))]
		;; (lambda (a) (value-of body (ext-env x a env)))]

		[`(zero? ,n-exp) (zero? (value-of-ds n-exp env))]
		;;[`(boolean? ,test1) (if (value-of-ds test1 env) #t #f)]
		[`,b (guard (boolean? b)) b]
		[`(* ,n-exp1 ,n-exp2)
		 (* (value-of-ds n-exp1 env) (value-of-ds n-exp2 env))]
		[`(sub1 ,n-exp) (sub1 (value-of-ds n-exp env))]
		;;LET CASE
		[`(let ((,x ,a)) ,body) 
		 (let ((t (value-of-ds a env)))
		   (value-of-ds body (ext-env-ds x t env)))]
		[`(,rator ,rand)
;;		 ((value-of-ds rator env) (value-of-ds rand env))])))
		 (apply-closure-ds (value-of-ds rator env) (value-of-ds rand env))])))

(define apply-env-ds
  (lambda (env y)
    (pmatch env
	    [`() (errorf 'apply env "unbound identifier ~s" y) ]
	    [`((,x . ,a) . ,env) (if (eqv? x y) a (apply-env-ds env y))])))

(define empty-env-ds
  (lambda ()
    '()))

(define ext-env-ds
  (lambda (x a env)
    `((,x . ,a) . ,env)))

(define apply-closure-ds
  (lambda (clos a)
    (pmatch clos
     [`(closure ,x ,body ,env) 
      ;;(value-of-ds body (lambda (y) (if (eqv? x y) a (env y))))])))
      (value-of-ds body (ext-env-ds x a env))])))

(define closure-ds 
  (lambda (x body env) 
    `(closure ,x ,body ,env)))



;; ==============================================
;;          RUN TEST - PART I
;; ==============================================

(define run-tests
  (lambda ()
    (test "if-value-of-fn"
      (value-of-fn 
        '((lambda (x) (if (zero? x) 
                          12 
                          47)) 
           0) 
        (empty-env))
      12)    
    (test "let-a-value-of-fn"
      (value-of-fn
       '(let ([y (* 3 4)])
          ((lambda (x) (* x y)) (sub1 6)))
       (empty-env))
      60)
    (test "let-b-value-of-fn"
      (value-of-fn
       '(let ([x (* 2 3)])
          (let ([y (sub1 x)])
            (* x y)))
       (empty-env))
      30)
    (test "let-c-value-of-fn"
      (value-of-fn
       '(let ([x (* 2 3)])
          (let ([x (sub1 x)])
            (* x x)))
       (empty-env))
      25)
    (test "if-value-of-ds"
      (value-of-ds
        '((lambda (x) (if (zero? x) 
                          12 
                          47)) 
           0) 
        (empty-env))
      12)    
    (test "let-a-value-of-ds"
      (value-of-ds
       '(let ([y (* 3 4)])
          ((lambda (x) (* x y)) (sub1 6)))
       (empty-env))
      60)
    (test "let-b-value-of-ds"
      (value-of-ds
       '(let ([x (* 2 3)])
          (let ([y (sub1 x)])
            (* x y)))
       (empty-env))
      30)
    (test "let-c-value-of-ds"
      (value-of-ds
       '(let ([x (* 2 3)])
          (let ([x (sub1 x)])
            (* x x)))
       (empty-env))
      25)
))
 
;;(run-tests)


;; ================================================================
;;                    Part II
;; ================================================================

(define value-of-both
  (lambda (exp env)
    (pmatch exp
	    [`,x (guard (symbol? x)) (apply-env-ds env x)]
	    [`,n (guard (number? n)) n]
	    [`(if ,test ,conseq ,alt)
	     (if (value-of-both test env) 
		 (value-of-both conseq env) (value-of-both alt env))]
	    [`(zero? ,n-exp) (zero? (value-of-both n-exp env))]
	    [`,b (guard (boolean? b)) b]
	    [`(* ,n-exp1 ,n-exp2)
	     (* (value-of-both n-exp1 env) (value-of-both n-exp2 env))]
	    [`(sub1 ,n-exp) (sub1 (value-of-both n-exp env))]
	    [`(quote ()) '()]
	    [`(null? ,ls) (null? (value-of-both ls env))]
	    [`(cons ,a ,d) (cons (value-of-both a env) (value-of-both d env))]
	    [`(car ,ls) (car (value-of-both ls env))]
	    [`(cdr ,ls) (cdr (value-of-both ls env))]
	    [`(lambda (,x) ,body) 
	     (lambda (a env^) (value-of-both body (ext-env-ds x a env)))]
	    [`(d-lambda (,x) ,body) (lambda (a env^) (value-of-both body (ext-env-ds x a env^)))]
	    [`(let ((,x ,a)) ,body) 
	     (let ((t (value-of-both a env)))
	       (value-of-both body (ext-env-ds x t env)))]
	    [`(,rator ,rand) ((value-of-both rator env) (value-of-both rand env) env)])))

(define extend-env 
  (lambda (x a env)
    `((,x . ,a) . ,env)))


(define apply-closure-both
  (lambda (clos a env)
    (pmatch clos
     [`(closure ,x ,body ,env) 
      ;;(value-of-ds body (lambda (y) (if (eqv? x y) a (env y))))])))
      (value-of-both body (ext-env-ds x a env) env)])))

(define closure-both
  (lambda (x body env) 
    `(closure ,x ,body ,env)))



;; ===============================================================
;;                   Tests for part II
;; ==============================================================

(define run-tests2
  (lambda ()

(define let-lexical
  '(let ([x 2])
     (let ([f (lambda (e) x)])
       (let ([x 5])
         (f 0)))))
 
(define let-dynamic
  '(let ([x 2])
     (let ([f (d-lambda (e) x)])
       (let ([x 5])
         (f 0)))))
 
(test "let-test-lexical"
      (value-of-both let-lexical (empty-env))
      2)
 
(test "let-test-dynamic"
      (value-of-both let-dynamic (empty-env))
      5)
 
(test "map-test-lexical"
  (value-of-both
    '(let
       ([l (cons 1 (cons 2 (cons 3 '())))])
         ((map (lambda (e) (cons e l))) l))
    (extend-env
      'map
      (value-of-both
        '(let ([map (lambda (map)
                    (lambda (f)
                      (lambda (l)
                        (if (null? l) '()
                            (cons (f (car l)) (((map map) f) (cdr l)))))))])
             (map map)) (empty-env)) (empty-env)))
  '((1 1 2 3) (2 1 2 3) (3 1 2 3)))
 
(test "map-test-dynamic"
  (value-of-both
    '(let
       ([l (cons 1 (cons 2 (cons 3 '())))])
         ((map (d-lambda (e) (cons e l))) l))
    (extend-env
      'map
      (value-of-both
        '(let ([map (lambda (map)
                    (lambda (f)
                      (lambda (l)
                        (if (null? l) '()
                            (cons (f (car l)) (((map map) f) (cdr l)))))))])
             (map map)) (empty-env)) (empty-env)))
  '((1 1 2 3) (2 2 3) (3 3)))
 
;; Notice the behavior of let in this next example.
;; we get letrec for free. (This is not a good thing.)
(test "map-test-dynamic"
  (value-of-both
    '(let
       ([map (d-lambda (f)
               (d-lambda (l)
                 (if (null? l) '()
                     (cons (f (car l)) ((map f) (cdr l))))))])
        (let ([f (d-lambda (e) (cons e l))])
          ((map f) (cons 1 (cons 2 (cons 3 '()))))))
    (empty-env))
  '((1 1 2 3) (2 2 3) (3 3)))

))