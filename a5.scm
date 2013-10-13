;; Kevin Lu 
;; September 27, 2013
;; a5.scm C311
;; call by value, call by name , call by reference, call by need

(load "pmatch.scm")
(load "test.scm")

(define apply-env
  (lambda (env y)
    ;(pmatch env
	    ;[`() (errorf 'apply env "unbound identifier ~s" y) ]
	    ;[`((,x . ,a) . ,env) (if (eqv? x y) a (apply-env env y))])))
    (env y)))

(define empty-env
  (lambda ()
    '()))

(define ext-env
  (lambda (x a env)
    ;`((,x . ,a) . ,env)))
    (lambda (y) (if (eqv? x y) a (env y)))))

(define apply-closure
  (lambda (clos a)
    ;(pmatch clos
    ; [`(closure ,x ,body ,env) 
      ;;(value-of-ds body (lambda (y) (if (eqv? x y) a (env y))))])))
     ; (value-of body (ext-env x a env))])))
    (clos a)))

(define closure-cbr 
  (lambda (x body env) 
   ; `(closure ,x ,body ,env)))
    (lambda (a) (val-of-cbr body (lambda (y) (if (eqv? x y) a (env y)))))))

(define closure-cbv
  (lambda (x body env)
   ; `(closure ,x ,body ,env)))
   (lambda (a) (val-of-cbv body (lambda (y) (if (eqv? x y) a (env y)))))))

(define closure-cbname 
  (lambda (x body env)
    (lambda (a) (val-of-cbname body (lambda (y) (if (eqv? x y) a (env y)))))))

(define closure-cbneed 
  (lambda (x body env)
    (lambda (a) (val-of-cbneed body (lambda (y) (if (eqv? x y) a (env y)))))))


(define val-of-cbr
  (lambda (exp env)
    (pmatch exp
	     [`,b (guard (boolean? b)) b]
	     [`,n (guard (number? n)) n]
	     [`(zero? ,n) (zero? (val-of-cbr n env))]
	     [`(sub1 ,n) (sub1 (val-of-cbr n env))]
	     [`(* ,n1 ,n2) (* (val-of-cbr n1 env) (val-of-cbr n2 env))]
	     [`(if ,test ,conseq ,alt) (if (val-of-cbr test env)
					   (val-of-cbr conseq env)
					   (val-of-cbr alt env))]
	     [`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
	     [`(set! ,x ,e) (guard (symbol? x ))
			    (set-box! (apply-env env x) (val-of-cbr e env))]
	     [`(random ,n) (random (val-of-cbr n env))]
	     [`,x (guard (symbol? x)) (unbox (apply-env env x))]
	     [`(lambda (,x) ,body) (closure-cbr x body env)]
	     [`(,rator ,x) 
	      (guard (symbol? x)) ((val-of-cbr rator env) (apply-env env x))]
	     [`(,rator ,rand) 
	      (apply-closure (val-of-cbr rator env) (box (val-of-cbr rand env)))])))

(define val-of-cbv
  (lambda (exp env)
    (pmatch exp
	    [`,b (guard (boolean? b)) b]
	    [`,n (guard (number? n)) n]
	    [`(zero? ,n) (zero? (val-of-cbv n env))]
	    [`(sub1 ,n) (sub1 (val-of-cbv n env))]
	    [`(* ,n1 ,n2) (* (val-of-cbv n1 env) (val-of-cbv n2 env))]
	    [`(if ,test ,conseq ,alt) (if (value-of-cbv test env)
                                  (val-of-cbv conseq env)
                                  (val-of-cbv alt env))]
	    [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
	    [`(set! ,x ,e) (guard (symbol? x))
	      (set-box! (apply-env env x) (val-of-cbv e env))]	     
	    ;[`(set! ,x ,rhs) (let ([vrhs (val-of-cbv rhs env)]) 
			    ; (set-box! (env x) vrhs))]
	    [`(random ,n) (random (val-of-cbv n env))]
	    [`,x (guard (symbol? x)) (unbox (apply-env env x))]
	    [`(lambda (,x) ,body) (closure-cbv x body env)]
	   ;  (lambda (a) (val-of-cbr body (ext-env x a env)))]
	    [`(,rator ,x) (guard (symbol? x)) 
	     ((val-of-cbv rator env) (box (unbox (apply-env env x))))]
	    [`(,rator ,rand) ((val-of-cbv rator env)
					        (box (val-of-cbv rand env)))])))



(define val-of-cbname
  (lambda (exp env)
    (pmatch exp
	    [`,b (guard (boolean? b)) b]
	    [`,n (guard (number? n)) n]
	    [`(zero? ,n) (zero? (val-of-cbname n env))]
	    [`(sub1 ,n) (sub1 (val-of-cbname n env))]
	    [`(* ,n1 ,n2) (* (val-of-cbname n1 env) (val-of-cbname n2 env))]
	    [`(+ ,n1 ,n2) (+ (val-of-cbname n1 env) (val-of-cbname n2 env))]
	    [`(if ,test ,conseq ,alt) (if (val-of-cbname test env)
                                  (val-of-cbname conseq env)
                                  (val-of-cbname alt env))]
	   ;; [`(begin2 ,e1 ,e2) (begin (value-of e1 env) (value-of e2 env))]
	    [`(random ,n) (random (val-of-cbname n env))]
	    [`,x (guard (symbol? x)) ((unbox (apply-env env x)))]
	    [`(lambda (,x) ,body) (closure-cbname x body env)]
	    [`(,rator ,x) (guard (symbol? x)) 
	     ((val-of-cbneed rator env) (apply-env env x))]
	    [`(,rator ,rand) ((val-of-cbname rator env) 
			      (box (lambda () (val-of-cbname rand env))))])))

	     ;(apply-closure (val-of-cbname rator env)
	     ;(val-of-cbname rand env))])))


(define unbox/need
  (lambda (b)
    (let ([val ((unbox b))]) 
	(set-box! b (lambda () val)) 
	val)))

;; (define val-of-cbneed
;;   (lambda (exp env)
;;     (pmatch exp
;; 	     [`,b (guard (boolean? b)) b]
;; 	     [`,n (guard (number? n)) n]
;; 	     [`(zero? ,n) (zero? (val-of-cbneed n env))]
;; 	     [`(sub1 ,n) (sub1 (val-of-cbneed n env))]
;; 	     [`(* ,n1 ,n2) (* (val-of-cbneed n1 env) (val-of-cbneed n2 env))]
;; 	     [`(if ,test ,conseq ,alt) (if (val-of-cbneed test env)
;; 					   (val-of-cbneed conseq env)
;; 					   (val-of-cbneed alt env))]
;; 	    ;; [`(begin2 ,e1 ,e2) (begin (value-of e1 env) (value-of e2 env))]
;; 	     [`(random ,n) (random (val-of-cbneed n env))]
;; 	     [`,x (guard (symbol? x)) (unbox (apply-env env x))] 
;; 	     [`(lambda (,x) ,body) (closure-cbneed x body env)]
;; 	     [`(,rator ,x) (guard (symbol? x)) 
;; 	     ((val-of-cbname rator env) (apply-env env x))]
;; 	     [`(,rator ,rand) 
;; 	      (apply-closure (val-of-cbneed rator env) 
;; 			     (box (lambda () (val-of-cbneed rand env))))])))
					    


(define val-of-cbneed
  (lambda (exp env)
    (pmatch exp
	     [`,b (guard (boolean? b)) b]
	     [`,n (guard (number? n)) n]
	     [`(zero? ,n) (zero? (val-of-cbneed n env))]
	     [`(sub1 ,n) (sub1 (val-of-cbneed n env))]
	     [`(* ,n1 ,n2) (* (val-of-cbneed n1 env) (val-of-cbneed n2 env))]
	     [`(if ,test ,conseq ,alt) (if (val-of-cbneed test env)
					   (val-of-cbneed conseq env)
					   (val-of-cbneed alt env))]
	     [`(random ,n) (random (val-of-cbneed n env))]
	     [`,x (guard (symbol? x)) (unbox/need (apply-env env x))]
	     [`(lambda (,x) ,body) (closure-cbneed x body env)]
	     [`(,rator ,x) (guard (symbol? x)) 
	     ((val-of-cbneed rator env) (apply-env env x))]
	     [`(,rator ,rand) 
	      (apply-closure (val-of-cbneed rator env) 
			     (box (lambda () (val-of-cbneed rand env))))])))


(define closure-cbden 
  (lambda (x body env)
    (lambda (a) (val-of-cbden body (lambda (y) (if (eqv? x y) a (env y)))))))



;; ===================================================
;;              PART II
;; ====================================================

(define val-of-cbden
  (lambda (exp env)
    (pmatch exp
	    [`,b (guard (boolean? b)) b]
	    [`,b (guard (boolean? b)) b]
	    [`,n (guard (number? n)) n]
	    [`(zero? ,n) (zero? (val-of-cbden n env))]
	    [`(sub1 ,n) (sub1 (val-of-cbden n env))]
	    [`(* ,n1 ,n2) (* (val-of-cbden n1 env) (val-of-cbden n2 env))]
	    [`(+ ,n1 ,n2) (+ (val-of-cbden n1 env) (val-of-cbden n2 env))]
	    [`(if ,test ,conseq ,alt) (if (val-of-cbden test env)
					  (val-of-cbden conseq env)
					  (val-of-cbnden alt env))]
	    ;; [`(begin2 ,e1 ,e2) (begin (value-of e1 env) (value-of e2 env))]
	    [`(random ,n) (random (val-of-cbden n env))]
	    [`,x (guard (symbol? x)) 
		   (lambda (env) (unbox (apply-env env x)))]
	   ;((unbox (apply-env env x)))]  ;; (lambda (env)
	   [`(lambda (,x) ,body) (closure-cbden x body env)]
	   ;[`(,rator ,x) (guard (symbol? x)) 
	   ;((val-of-cbden rator env) (apply-env env x))]
	    [`(,rator ,rand) 
	     (apply-closure (val-of-cbden rator env) 
			    (box (lambda (env) (val-of-cbden rand env))))])))





















;;====================================================
;;            PART I TESTS
;;====================================================
 
(define run-tests
  (lambda ()
    (test "set!"
  (val-of-cbr
   '((lambda (x) (begin2 (set! x #t)
                         (if x 3 5))) #f)
   (empty-env))
  3)
 
;Returns 4 under CBR...
(test "interesting-cbr-1"
  (val-of-cbr
   '((lambda (a)
       ((lambda (p)
          (begin2
           (p a)
           a)) (lambda (x) (set! x 4)))) 3)
   (empty-env))
  4)
 
;...but returns 3 under CBV.
(test "interesting-cbv-1"
  (val-of-cbv
   '((lambda (a)
       ((lambda (p)
          (begin2
           (p a)
           a)) (lambda (x) (set! x 4)))) 3)
   (empty-env))
  3)
 
;; returns 44 under CBR...
(test "interesting-cbr-2"
  (val-of-cbr
   '((lambda (f)
       ((lambda (g)
          ((lambda (z) (begin2
                        (g z)
                        z))
           55))
        (lambda (y) (f y)))) (lambda (x) (set! x 44)))
   (empty-env))
  44)
 
;; ...but returns 55 under CBV!  You can change the "begin2" to
;; "begin" and evaluate this in the Scheme REPL as evidence that
;; Scheme uses CBV.
(test "interesting-cbv-2"
  (val-of-cbv
   '((lambda (f)
       ((lambda (g)
          ((lambda (z) (begin2
                        (g z)
                        z))
           55))
        (lambda (y) (f y)))) (lambda (x) (set! x 44)))
   (empty-env))
  55)
 
;; Returns 44 under CBR...
(test "interesting-cbr-3"
  (val-of-cbr
   '((lambda (swap)
       ((lambda (a)
          ((lambda (b)
             (begin2
              ((swap a) b)
              a)) 44)) 33))
     (lambda (x)
       (lambda (y)
         ((lambda (temp)
            (begin2
             (set! x y)
             (set! y temp))) x))))
   (empty-env))
  44)
 
;; ...but returns 33 under CBV.
(test "interesting-cbv-3"
  (val-of-cbv
   '((lambda (swap)
       ((lambda (a)
          ((lambda (b)
             (begin2
              ((swap a) b)
              a)) 44)) 33))
     (lambda (x)
       (lambda (y)
         ((lambda (temp)
            (begin2
             (set! x y)
             (set! y temp))) x))))
   (empty-env))
  33)
))

(define random-sieve
  '((lambda (n)
      (if (zero? n)
          (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) #t #f) #f) #f) #f) #f) #f)
          (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f #t))))))))
    (random 2)))
 

(define run-tests2
  (lambda ()
    ;; call-by-name
;;P(false positive) <= .01                                                     
(test "random-by-name"
  (val-of-cbname random-sieve (empty-env)) #f)
 
;; call-by-need                                                                  
(test "random-by-need"
  (val-of-cbneed random-sieve (empty-env)) #t)
 
;; Does not terminate with val-of-cbr or val-of-cbv -- try it!
(test "omega-by-name"
  (val-of-cbname
   '((lambda (z) 100)
     ((lambda (x) (x x)) (lambda (x) (x x))))
   (empty-env))
  100)
))


;; ===================================================
;;     TEST FOR PART II
;; ===================================================

(define run-tests3
  (lambda ()
    (define convert
  (lambda (expr)
    (pmatch expr
      [`,atom (guard (not (pair? atom))) atom]
      [`(let ([,x ,e]) ,body) `((lambda (,x) ,(convert body)) ,(convert e))]
      [`,otherwise (cons (convert (car expr)) (convert (cdr expr)))])))

(test "cbname-1"
  (val-of-cbname
   (convert '(let ([f (lambda (y)
                        (let ([x 0])
                          y))])
               (let ([x 1])
                 (f (+ x 1)))))
   (empty-env))
  2)

(test "cbden-1"
  (val-of-cbden
   (convert '(let ([f (lambda (y)
                        (let ([x 0])
                          y))])
               (let ([x 1])
                 (f (+ x 1)))))
   (empty-env))
  1)

(test "cbname-2"
  (val-of-cbname
   (convert '(let ([f (lambda (y)
                        (let ([x 0])
                          y))])
               (let ([x 1])
                 (f (* 2 x)))))
   (empty-env))
  2)

(test "cbden-2"
  (val-of-cbden
   (convert '(let ([f (lambda (y)
                        (let ([x 0])
                          y))])
               (let ([x 1])
                 (f (* 2 x)))))
   (empty-env))
  0)

(test "cbname-3"
  (val-of-cbname
   (convert '(let ([x 1])
               (let ([f (lambda (y) (* x y))])
                 (let ([x 3])
                   (f (+ x 1))))))
   (empty-env))
  4)

(test "cbden-3"
  (val-of-cbden
   (convert '(let ([x 1])
               (let ([f (lambda (y) (* x y))])
                 (let ([x 3])
                   (f (+ x 1))))))
   (empty-env))
  12)

(test "cbname-4"
  (val-of-cbname
   (convert '(let ([x 1])
               (let ([f (lambda (y) (* x (let ([x 2]) y)))])
                 (let ([x 3])
                   (f (+ x 1))))))
   (empty-env))
  4)

(test "cbden-4"
  (val-of-cbden
   (convert '(let ([x 1])
               (let ([f (lambda (y) (* x (let ([x 2]) y)))])
                 (let ([x 3])
                   (f (+ x 1))))))
   (empty-env))
  9)

(test "cbname-5"
  (val-of-cbname
   (convert '(let ([f (lambda (x)
                        (let ([a 0])
                          (+ a x)))])
               (let ([a 10])
                 (f a))))
   (empty-env))
  10)

(test "cbden-5"
  (val-of-cbden
   (convert '(let ([f (lambda (x)
                        (let ([a 0])
                          (+ a x)))])
               (let ([a 10])
                 (f a))))
   (empty-env))
  0)

(test "cbname-6"
  (val-of-cbname
   (convert '(let ([x 0])
               (let ([f (lambda (y) (+ x y))])
                 (let ([x 1])
                   (f (+ x 1))))))
   (empty-env))
  2)

(test "cbden-6"
  (val-of-cbden
   (convert '(let ([x 0])
               (let ([f (lambda (y) (+ x y))])
                 (let ([x 1])
                   (f (+ x 1))))))
   (empty-env))
  3)

;; This last test is particularly weird.  Under pretty much any other
;; evaluation strategy, the result would be an error since the last x
;; is unbound.  But under call-by-denotation, it evaluates to 3!
(test "cbden-weird"
  (val-of-cbden
   (convert '((let ([x 3]) (lambda (y) y)) x))
   (empty-env))
  3)
))



