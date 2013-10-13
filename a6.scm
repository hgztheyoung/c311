;;Kevin Lu 
;;Oct. 3, 2013
;;a6.scm

(load "test.scm")
(load "smart-lambda.ss")

(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only 
	    (errorf 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))

(define empty-k 
  (lambda ()
    (lambda (v) v)))



;;1
(define times
  (lambda (ls)
    (cond
      [(null? ls) 1]
      [(zero? (car ls)) 0]
      [else (* (car ls) (times (cdr ls)))])))
 
(define times-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) (k 0)]
      ;[else (* (car ls) (timeOBs (cdr ls)))])))
      [else (times-cps (cdr ls) (lambda (v) (k (* (car ls) v))))])))



;;2
(define times-cps-shortcut
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) 0]
      ;[else (* (car ls) (timeOBs (cdr ls)))])))
      [else (times-cps (cdr ls) (lambda (v) (k (* (car ls) v))))])))


;;3 Works by using ((plus-cps 9 (empty-k)) 8 (empty-k)) => 17
(define plus
  (lambda (m)
    (lambda (n)
      (+ m n))))


(define plus-cps
  (lambda (m k)
    (k (lambda (n)
	 (k (+ m n))))))
      ;(plus-cps m (lambda (c) (plus-cps n (lambda (x) (k (+ c x)))))))))
      ;(plus-cps (k (+ m n))))))

;;4
(define count-syms*
  (lambda (ls)
    (cond
      [(null? ls) 0]
      [(pair? (car ls)) (+ (count-syms* (car ls)) (count-syms* (cdr ls)))]
      [(symbol? (car ls)) (add1 (count-syms* (cdr ls)))]
      [else (count-syms* (cdr ls))])))

(define count-syms*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 0)]
      ;[(pair? (car ls)) (+ (count-syms* (car ls)) (count-syms* (cdr ls)))]
      ;[(symbol? (car ls)) (add1 (count-syms* (cdr ls)))]
      ;[else (count-syms* (cdr ls))])))
      [(pair? (car ls)) 
       (count-syms*-cps  (cdr ls) 
			 (lambda (x) (k (+ x (car ls)))))]
      [(symbol? (car ls)) 
       (count-syms*-cps (cdr ls) 
			(lambda (x) (k (add1 x))))]
      [else (count-syms*-cps (cdr ls) k)])))

;;5 
(define cons-cell-count
  (lambda (ls)
    (cond
      [(pair? ls) 
       (add1 (+ (cons-cell-count (car ls)) 
		(cons-cell-count (cdr ls))))]
      [else 0])))


(define cons-cell-count-cps
  (lambda (ls k)
    (cond
     [(pair? ls) 
      (cons-cell-count-cps (cdr ls) 
	      (lambda (c) (cons-cell-count-cps (car ls) 
		    (lambda (x)
		        (k (add1 (+ x c)))))))]			       
     [else (k 0)])))


;;6
(define walk
  (lambda (v ls)
    (cond
      [(symbol? v)
       (let ((p (assq v ls)))
         (cond
           [p (walk (cdr p) ls)]
           [else v]))]
      [else v])))
	  
;; works but I am not sure...			
(define walk-cps
  (lambda (v ls k)
    (cond
     [(symbol? v)
      (let ((p (assq v ls)))
         (cond
	;[p (walk (cdr p) ls )]
	[p (walk-cps (cdr p) ls k)]
	  [else (k v)]))]
     [else (k v)])))
	  	  
;;7 
;;ack: computes the Ackermann function
;; (http://en.wikipedia.org/wiki/Ackermann_function).  Warning: if you
;; run this program with m >= 4 and n >= 2, you'll be in for a long
;; wait.
(define ack
  (lambda (m n)
    (cond
      [(zero? m) (add1 n)]
      [(zero? n) (ack (sub1 m) 1)]
      [else (ack (sub1 m)
                 (ack m (sub1 n)))])))

(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      ;[(zero? n) (ack (sub1 m) 1)]
      ;[else (ack (sub1 m)
      ;
      ;(ack m (sub1 n)))])))
      [(zero? n) (ack-cps (sub1 m) 1 k)]
      [else (ack-cps m (sub1 n) (lambda (v) (ack-cps (sub1 m) v k)))])))
	  

;8
(define fib
  (lambda (n)
    ((lambda (fib)
       (fib fib n))
     (lambda (fib n)
       (cond
	 [(zero? n) 0]
	 [(= 1 n) 1]
	 [else (+ (fib fib (sub1 n)) (fib fib (sub1 (sub1 n))))])))))


(define fib-cps
  (lambda (n k)
    ((lambda (fib-cps k)
       (fib-cps fib-cps n k))
     (lambda (fib-cps n k)
       (cond
	 [(zero? n) (k 0)]
	 [(= 1 n) (k 1)]
	 [else (fib-cps fib-cps 
	     (sub1 n) (lambda (s) (fib-cps fib-cps (sub1 (sub1 n)) 
		     (lambda (s2) (k (+ s s2))))))])) k)))



;9
(define pascal
  (lambda (n)
    (let ((pascal
           (lambda (pascal)
             (lambda (m a)
               (cond
                 [(> m n) '()]
                 [else (let ((a (+ a m)))
                         (cons a ((pascal pascal) (add1 m) a)))])))))
      ((pascal pascal) 1 0))))


(define pascal-cps
  (lambda (n k)
    (let ((pascal-cps
           (lambda (pascal-cps k)
            (k (lambda (m a k)
               (cond
                 [(> m n) (k '())]
                 [else (let ([a (+ a m)])
			 (pascal-cps pascal-cps (lambda (f2) (f2 (add1 m) a (lambda (b) (k (cons a b)))))))]))))))
                         ;(cons a ((pascal pascal) (add1 m) a)))])))))		       
	  (pascal-cps pascal-cps (lambda (f) (f 1 0 k))))))

;10
(define empty-s
  (lambda ()
    '()))
 
(define extend-s
  (lambda (x v s)
    (cons `(,x . ,v) s)))
 
(define unify
  (lambda (v w s)
    (let ([v (walk v s)])
      (let ([w (walk w s)])
        (cond
          [(eq? v w) (k s)]
          [(symbol? v) (extend-s v w s)]
          [(symbol? w) (extend-s w v s)]
          [(and (pair? v) (pair? w))
           (let ((s (unify (car v) (car w) s)))
             (cond
               [s (unify (cdr v) (cdr w) s)]
               [else #f]))]
          [(equal? v w) (k s)]
          [else #f])))))

;================================================================
;================================================================


(define empty-s-cps
  (lambda (k)
    (k '())))
 
(define extend-s-cps
  (lambda (x v s k)
    (k (cons `(,x . ,v) s))))
 
(define unify-cps
  (lambda (v w s k)
    (let ([v (walk-cps v s k)])
      (let ([w (walk-cps w s k)])
        (cond
          [(eq? v w) (k s)]
          [(symbol? v) (extend-s-cps v w s k)]
          [(symbol? w) (extend-s-cps w v s k)]
          [(and (pair? v) (pair? w))
           (let ((s (unify-cps (car v) (car w) s k)))
             (cond
               [s (unify-cps (cdr v) (cdr w) s k)]
               [else #f]))]
          [(equal? v w) (k s)]
          [else #f])))))




;11
(define M
  (lambda (f)
    (lambda (ls)
      (cond
        ((null? ls) '())
        (else (cons (f (car ls)) ((M f) (cdr ls))))))))

;12
(define use-of-M
  ((M (lambda (n) (add1 n))) '(1 2 3 4 5)))
	
;;=================================================================
;;=================================================================

(define M-cps
  (lambda (f k)
    (k (lambda (ls k)
	 (cond
	  [(null? ls) (k'())]
	  [else (M-cps f (lambda (v) (v (cdr ls) 
					(lambda (b) (f (car ls) 
						       (lambda (f2) (k (cons f2 b))))))))])))))

;12
(define use-of-M-cps
  ;((M (lambda (n k) (add1 n))) '(1 2 3 4 5)) (empty-k)))
  (M-cps (lambda (n k) (k (add1 n))) (lambda (f) (f '(1 2 3 4 5) (empty-k)))))