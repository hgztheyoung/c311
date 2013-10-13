0;136;0c;B0;136;0c0;136;0c
(load "test.scm") 
(load "pmatch.scm")
;;Kevin Lu a3.scm

;; ========================================================
;; Value-of 
;; =========================================================

(define value-of
  (lambda (exp env)
    (pmatch exp
	    [`,x (guard (symbol? x)) (env x)]
	    [`,n (guard (number? n)) n]
	    [`(if ,test ,conseq ,alt) 
	      (if (value-of test env) (value-of conseq env) (value-of alt env))]
	    [`(lambda (,x) ,body) 
	     (lambda (a) (value-of body (ext-env-fn x a env)))]
				   ;;(if (eqv? x y) a (env y))))] 
	    [`(zero? ,n-exp) (zero? (value-of n-exp env))]
	    ;;[`(boolean? ,test1) (if (value-of test1 env) #t #f)]
	    [`,b (guard (boolean? b)) b]
	    [`(* ,n-exp1 ,n-exp2) 
	     (* (value-of n-exp1 env) (value-of n-exp2 env))]
	    [`(sub1 ,n-exp) (sub1 (value-of n-exp env))]
	    ;;LET CASE
	    [`(let ((,x ,a)) ,body) (let ((t (value-of a env)))
	     (value-of body (ext-env-fn x t env)))] ;;(lambda (y) (....)
	       ;;(value-of body (ext-env-fn x a env)))]
	    [`(,rator ,rand) 
	     ((value-of rator env) (value-of rand env))])))


(define empty-env 
  (lambda ()
    '()))

;; ====================================================================
;; Functional
;; ===================================================================

(define value-of-fn
  (lambda (exp env)
    (pmatch-who "value-of-fn" exp
	    [`,x (guard (symbol? x)) (apply-env-fn env x)]	   
	    [`,n (guard (number? n)) n]
	    [`(if ,test ,conseq ,alt) 
	     (if (value-of-fn test env) (value-of-fn conseq env) (value-of-fn alt env))]
	    ;;[`(lambda () ,body) (empty-env-fn)]
	    [`(lambda (,x) ,body) 
	    (lambda (a) (value-of-fn body (ext-env-fn x a env)))]
	     ;;(lambda (a) (value-of body (ext-env x a env)))]
	    [`(zero? ,n-exp) (zero? (value-of-fn n-exp env))]
	    ;;[`(boolean? ,test1) (if (value-of-fn test1 env) #t #f)]
	    [`,b (guard (boolean? b)) b]
	    [`(* ,n-exp1 ,n-exp2) 
	     (* (value-of-fn n-exp1 env) (value-of-fn n-exp2 env))]
	    [`(sub1 ,n-exp) (sub1 (value-of-fn n-exp env))]
	    ;;LET CASE
	    [`(let ((,x ,a)) ,body) (let ((t (value-of-fn a env)))
	     (value-of-fn body (ext-env-fn x t env)))]
	    [`(,rator ,rand) 
	     ;;(ext-env-fn x a env)])))
	      ((value-of-fn rator env) (value-of-fn rand env))])))

(define empty-env-fn 
  (lambda ()
    (lambda (y) (errorf 'empty env "unbound indentifier" y))))

(define ext-env-fn
  (lambda (x a env)
    (lambda (y) (if (eqv? x y) a (env y)))))

(define apply-env-fn
  (lambda (env x)
    (env x)))


;; (define apply-env-fn
;;   (lambda (env y)
;;     (pmatch 
;;      [`(empty-env-fn env) '() ] ;; fix this
;;      [`(apply-env-fn ,x ,a ,env) (if (eqv? x y) a (apply-env env y))])))
    

;; (define ext-env-fn 
;;   (lambda (x a env)
;;     `(ext-env ,x ,a ,env)))

;; (define empty-env-fn 
;;   (lambda () 
;;     `(empty-env)))

;= =======================================================
;; Structural data
;; ========================================================

(define value-of-ds
  (lambda (exp env)
    (pmatch-who "value-of-ds" exp
	    [`,x (guard (symbol? x)) (apply-env-ds env x)]
	    [`,n (guard (number? n)) n]
	    [`(if ,test ,conseq ,alt) 
	      (if (value-of-ds test env) (value-of-ds conseq env) (value-of-ds alt env))]
	    ;;[`(lambda () ,body) (empty-env-ds)]
	    [`(lambda (,x) ,body) 
	      (lambda (a) (value-of-ds body (ext-env-ds x a env)))] 
	    ;; (lambda (a) (value-of body (ext-env x a env)))]
	    [`(zero? ,n-exp) (zero? (value-of-ds n-exp env))]
	    ;;[`(boolean? ,test1) (if (value-of-ds test1 env) #t #f)]
	    [`,b (guard (boolean? b)) b]
	    [`(* ,n-exp1 ,n-exp2) 
	     (* (value-of-ds n-exp1 env) (value-of-ds n-exp2 env))]
	    [`(sub1 ,n-exp) (sub1 (value-of-ds n-exp env))]
	    ;;LET CASE
	    [`(let ((,x ,a)) ,body) (let ((t (value-of-ds a env)))
	     (value-of-ds body (ext-env-ds x t env)))]
	    [`(,rator ,rand) 
	     ;;(ext-env-ds x a env)])))
	     ((value-of-ds rator env) (value-of-ds rand env))])))

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

;; =============================================================
;; fo-eulav
;; =============================================================
(define fo-eulav 
  (lambda (x env)
     (pmatch exp
	    ;; [`,x (guard (symbol? x)) (env x)]
	    ;; [`,n (guard (number? n)) n]
	    ;; [`(if ,test ,conseq ,alt) 
	    ;;   (if (value-of test env) (value-of conseq env) (value-of alt env))]
	    ;; [`(lambda (,x) ,body) 
	    ;;  (lambda (a) (value-of body (ext-env-fn x a env)))]		
	    ;; [`(zero? ,n-exp) (= (value-of n-exp env) 0)]
	    ;; [`(boolean? ,test1) (if (value-of test1 env) #t #f)]
	    ;; [`(* ,n-exp1 ,n-exp2) 
	    ;;  (* (value-of n-exp1 env) (value-of n-exp2 env))]
	    ;; [`(sub1 ,n-exp) (- (value-of n-exp env) 1)]
	    ;; [`(let ((,x ,a)) ,body) (let ((t (value-of a env)))
	    ;;  (value-of body (ext-env-fn x t env)))]
	    ;; [`(,rator ,rand) 
	    ;;  ((value-of rator env) (value-of rand env))])))
	     [`,x (guard (symbol? x)) (env x)]
	     [`,n (guard (number? n)) n]
	     ;;[(guard (symbol? x)) `,x x]
	     ;;[(guard (number? n)) `,n n]
	     [`(,alt ,conseq ,test fi)
	      (if (value-of test env) (value-of conseq env) 
		  (value-of alt env))]
	    ;; [`(?orez ,n-exp) (= (value-of n-exp env) 0)]
	     [`(,body (,x) adbmal) 
	      (lambda (a) (value-of body (ext-env-fn x a env)))]
	     [`(,n-exp ?orez) (zero? (value-of n-exp env))]
	     [`(,n-exp 1bus) (sub1 (value-of n-exp env))]
	     [`,b (guard (boolean? b)) b]
	     [`(,rator ,rand) 
	      ((value-of rator env) (value-of rand env))])))




;; ================================================================
;; ================================================================

;; (define apply-env-ds
;;   (lambda (env y)
;;     (pmatch 
;;      [`(empty-env-fn env) '() ]
;;      [`(apply-env-fn ,x ,a ,env) (if (eqv? x y) a (apply-env env y))])))
    

;; (define ext-env-ds
;;   (lambda (x a env)
;;     `(ext-env ,x ,a ,env)))

;; (define empty-env-ds 
;;   (lambda () 
;;     `(empty-env)))



;; ============================================================
;;           code for running tests
;;           load and run, test should show up
;; =============================================================
(define run-tests
  (lambda ()
    (test "if-value-of"
      (value-of 
        '((lambda (x) (if (zero? x) 
                          12 
                          47)) 
           0) 
        (lambda (y) (errorf 'value-of "unbound variable ~s" y)))
      12)    
    (test "let-a-value-of"
      (value-of
       '(let ([y (* 3 4)])
          ((lambda (x) (* x y)) (sub1 6)))
       (lambda (y) (errorf 'value-of "unbound variable ~s" y)))
      60)
    (test "let-b-value-of"
      (value-of
       '(let ([x (* 2 3)])
          (let ([y (sub1 x)])
            (* x y)))
       (lambda (y) (errorf 'value-of "unbound variable ~s" y)))
      30)
    (test "let-c-value-of"
      (value-of
       '(let ([x (* 2 3)])
          (let ([x (sub1 x)])
            (* x x)))
       (lambda (y) (errorf 'value-of "unbound variable ~s" y)))
      25) 
    (test "let-d-value-of"
      (value-of 
       '(let ((! (lambda (x) (* x x))))
          (let ((! (lambda (n)
		          (if (zero? n) 1 (* n (! (sub1 n)))))))
	        (! 5)))
       (lambda (y) (errorf 'value-of "unbound variable ~s" y)))
      80)
    (test "poormans-fact-value-of"
      (value-of
       '(((lambda (f)
            (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
          (lambda (f)
            (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
         5)
       (lambda (y) (errorf 'value-of "unbound variable ~s" y)))
      120)
    (test "if-value-of-fn"
      (value-of-fn 
        '((lambda (x) (if (zero? x) 
                          12 
                          47)) 
           0) 
        (empty-env-fn))
      12)    
    (test "let-a-value-of-fn"
      (value-of-fn
       '(let ([y (* 3 4)])
          ((lambda (x) (* x y)) (sub1 6)))
       (empty-env-fn))
      60)
    (test "let-b-value-of-fn"
      (value-of-fn
       '(let ([x (* 2 3)])
          (let ([y (sub1 x)])
            (* x y)))
       (empty-env-fn))
      30)
    (test "let-c-value-of-fn"
      (value-of-fn
       '(let ([x (* 2 3)])
          (let ([x (sub1 x)])
            (* x x)))
       (empty-env-fn))
      25)
    (test "poormans-fact-value-of-fn"
      (value-of-fn
       '(((lambda (f)
            (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
          (lambda (f)
            (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
         5)
       (empty-env-fn))
      120)
    (test "if-value-of-ds"
      (value-of-ds
        '((lambda (x) (if (zero? x) 
                          12 
                          47)) 
           0) 
        (empty-env-ds))
      12)    
    (test "let-a-value-of-ds"
      (value-of-ds
       '(let ([y (* 3 4)])
          ((lambda (x) (* x y)) (sub1 6)))
       (empty-env-ds))
      60)
    (test "let-b-value-of-ds"
      (value-of-ds
       '(let ([x (* 2 3)])
          (let ([y (sub1 x)])
            (* x y)))
       (empty-env-ds))
      30)
    (test "let-c-value-of-ds"
      (value-of-ds
       '(let ([x (* 2 3)])
          (let ([x (sub1 x)])
            (* x x)))
       (empty-env-ds))
      25)
    (test "poormans-fact-value-of-ds"
      (value-of-ds
       '(((lambda (f)
            (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
          (lambda (f)
            (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
         5)
       (empty-env-ds))
      120)
 
))
;;(run-tests)				