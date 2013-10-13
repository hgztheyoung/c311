;;Kevin Lu
;; september 7, 2013
;; a2.scm
;; C311

;;(define value-of
;;  (lambda (exp env)
;;  (pmatch-who "value-of" exp
;;		[`,x (guard (symbol? x)) (env x)]
;;		[`(lambda (,x) ,body) (value-of body (lambda (a)
;;						       (if (eqv? x y) 
;;							   a 
;;							   (env y))))]
;;		[`(,rator ,rand) `((value-of rator env) 
;;				  (value-of (rand env)))])))

(load "pmatch.scm")

;;1
;;(define list-ref
;;  (lam+bda (ls n)
;;    (letrec
;;	((nth-cdr (lambda (n) 
;;		    
;;		    ))
;;      (car (nth-cdr n)))))


;;2
(define lambda->lumbda 
  (lambda (exp)
    (pmatch-who "convert lambda" exp
		[`,x (guard (symbol? x)) x]
		[`(lambda (,x) ,body) 
		 `(lumbda (,x) ,(lambda->lumbda body))]
		[`(,rator ,rand) 
		 `(,(lambda->lumbda rator) ,(lambda->lumbda rand))])))
	  
;;3
(define vars
  (lambda (exp)
    (pmatch-who "vars" exp
		[`,x (guard (symbol? x)) `(,x)]
		[`(lambda (,x) ,body) (vars body)]
		[`(,rator ,rand) (append (vars rator) (vars rand))])))


;;4 -----------------not working properly--------------------
;(define union
;  (lambda (ls1 ls2)
;    (pmatch-who "union" ls1
;		[`() ls2]
;		[`(,a . ,d) (guard (memv a ls2)) 
;		 (union (cdr ls1) ls2)]
;		[`(,a . ,d) (cons a (union d ls2))])))

;;4
(define union 
  (lambda (ls1 ls2)
    (pmatch-who "union" ls1
		[`() ls2]
		[`(,a . ,d) (guard (memv a ls2)) 
		 (union (cdr ls1) ls2) ]
		[`(,a . ,d) (cons a (union (cdr ls1) ls2))])))

;;5  ---------------not working properly---------------
(define unique-vars
  (lambda (exp)
    (pmatch-who "unique vars" exp
		[`,x (guard (symbol? x)) `(,x)]
		[`(lambda (,x) ,body) (unique-vars body)]
		[`(,rator ,rand) (union (unique-vars rator) (unique-vars rand))])))


;;6
;; (define extend 
;;   (lambda (x pred)
;;    (cond 
;;     [(if (eqv? (pred x) #f) #t]
;;     [(extend x pred])))

;;7
(define free? 
  (lambda (s exp)
    (pmatch-who "free?" exp
		[`,x (guard (symbol? x)) 
		       (eqv? s x)]
		[`(lambda (,x) ,body) 
		(if (and (eqv? s x) (memv s (unique-vars body))) 
		    #f
		    (free? s body))]     
		[`(,rator ,rand) (or (free? s rator) (free? s rand))])))

;;8 
(define bound?
   (lambda (s exp)
     (pmatch-who "bound?" exp
		 [`,x (guard (symbol? x)) #f]
		 [`(lambda (,x) ,body) 
		  (or 
		   (and (eqv? x s) (free? s body)) 
		  (bound? s body))]	   
		 [`(,rator ,rand) (or (bound? s rator) (bound? s rand))])))


;;9 
(define free
  (lambda (exp)
    (pmatch-who "free" exp
		[`,x (guard (symbol? x)) `(,x)]
		[`(lambda (,x) ,body) (memv x (unique-vars body))
		 (remv x (unique-vars body))]
		[`(,rator ,rand) (union (free rator) (free rand))])))


;;10;;-------------------------Almost got it....-----------------------
 (define bound
   (lambda (exp)
    (pmatch-who "bound" exp
		[`,x (guard (symbol? x))'()]
		[`(lambda (,x) ,body) 
		 ;;(remv (car (free body)) (memv x (unique-vars body)))]
		 ;; (memv x (unique-vars body))]
		 ;;(bound? x body)
		 (cons x (bound body))
		 ]
		[`(,rator ,rand) (union (bound rator) (bound rand))])))

;11
;; (define walk-symbol
;;   (lambda (x s)
;;     (pmatch-who "walk-symbol" exp
		

;; (define walk-symbol
;;   (lambda (x ls)
;;     (cond 
;;      [(assv x ls) (cadr (assv x ls))]
;;      [(symbol? (car (assv x ls))) (walk-symbol (cadr (assv x ls)))]
;;      [else (walk-symbol x ls)])))
		


;;12
;;=======================================================
;; Worked with Jaimie, Patrick and Rusestam
;; actually I didn't do much (contribute), patrick and hte other two
;; came up with most of the cases  
;; so this shouldn't be considered. 
;;======================================================
(define lex
  (lambda (exp acc)
    (pmatch-who "lex" exp
                [`,x (guard (symbol? x))
                     (if (assq x acc)

                     (list (cons 'var (list (cdr (assq x acc)))))
                     (list (cons 'free-var (list x))))]
                [`(lambda (,x) ,body)
                  (list (append (list 'lambda) (lex body

                                                    (cons (cons x 0) (map (lambda (x) (cons (car x) (add1 (cdr x)))) acc)))))]

                [`(,rator ,rand)
                  (append (lex rator acc) (lex rand acc))])

    ))
