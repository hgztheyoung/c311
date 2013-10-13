;; Kevin Lu 
;; C311 
;; september 2, 2013
;; a1.scm 

;;(define count-up
;;  (lambda (num num2) 
;;    (cond 
;;     [(zero? num2) (cons num '())]
;;     [else (cons num (count-up (+ 1 num) (- num2 1)))])))

;;1
(define count-down 
 (lambda (num)
   (cond 
    [(zero? num) (cons num '())]
    [else (cons num (count-down (- num 1)))])))

;;2
(define insertR 
  (lambda (s1 s2 ls)
    (cond 
     [(null? ls) '()]
     [(equal? (car ls) s1) (cons (car ls) (cons s2 (insertR s1 s2 (cdr ls))))]
     [else (cons (car ls) (insertR s1 s2 (cdr ls)))])))

;;3 
(define remv-1st 
  (lambda (symb ls)
    (cond 
     [(null? ls) '()]
     [(equal? (car ls) symb) (cdr ls)]
     [else (cons (car ls) (remv-1st symb (cdr ls)))])))

;;4
(define occurs-?s
  (lambda (ls)
    (cond 
     [(null? ls) 0]
     [(equal? (car ls) '?) (+ 1 (occurs-?s (cdr ls)))] 
     [else (occurs-?s (cdr ls))])))

;;5
(define filter  
  (lambda (pred ls)
    (cond 
     [(null? ls) '()]
     [(pred (car ls)) (cons (car ls) (filter pred (cdr ls)))]
     [else (filter pred (cdr ls))])))


;;6
(define zip
  (lambda (ls ls2)
    (cond 
     [(null? ls) (cons ls '())]
     [(null? (cdr ls)) (cons (cons (car ls) (car ls2)) '())]
     [else (cons (cons (car ls) (car ls2)) (zip (cdr ls) (cdr ls2)))])))

;;7
(define map 
  (lambda (p ls)
    (cond 
     [(null? ls) '()]
     [else (cons (p (car ls)) (map p (cdr ls)))])))

;;8 
(define append
  (lambda (ls ls2)
    (cond 
     [(null? ls) ls2]
     [(null? ls2) ls]
     [else (cons (car ls) (append (cdr ls)  ls2))])))

;;9
(define reverse 
  (lambda (ls)
    (cond 
     [(null? ls) '()]
     [(null? (cdr ls)) ls]
     [else (cons (car ls) (reverse (cdr ls)))]))) 
;; unsure how to implement
;; ************* unable to do *********

;;10
(define fact 
  (lambda (num)
    (cond 
     [(zero? num) 1]
     [else (* num (fact (sub1 num)))])))

;;************************needs work************
;;;**********************************************
;;11
(define member-?*
  (lambda (ls)
    (cond 
     [(null? ls) #f]
     [(equal? (car ls) '?) #t]
     [else (member-?* (cdr ls))])))
     
;;   [(= (occurs-?s ls) 0) (member-?* (cdr ls))]
;    [(not (equal? (car ls) '?)) (member-?* (cdr ls))]
;    [else #t])))

;;12
;(define fib 
;  (lambda (num)
;    (cond 
;     [(zero? num) 0]
;     [(= num 1) 1]
;     [(= num 2) 1]
;     [else (+ (fib  )])))

;;13
(define cons-cell-count
  (lambda (input)
    (cond
     [(null? input) 0]
     [(= (length input) 1) 0]
     [(symbol? input) 0]
     ;[(null? (cdr input)) 0]
     [else (+ 1 (cons-cell-count (cdr input)))])))
;; ***********incompelte********************


;;14
;; rewrite
;; (a b) and (a . (b. ())) are equal
;; rewrite  ((w x) y (z)) equivalent
;; ((w .(x .())) . (y .((z .()) .())))


;;15
;(define binary->natural 
;   (lambda (ls)
;     (cond 
;      [(null? ls) 0]
;      [ ]
;      [else ])))

;;16
;(define natural->binary
;  (lambda (num)
;    (cond
;     [(zero? num) '()]
;     [(= num 1) 1]
;     [(= (mod num 2) 0) (cons 1 (natural->binary (/ (sub1 num) 2)))]
;     [else (cons 0 (natural->binary (/ (sub1 num) 2)))])))

;; doesnt work
