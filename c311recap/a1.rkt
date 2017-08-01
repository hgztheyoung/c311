#lang racket
;;A1
(define (countdown n)
  (if (= n -1)
      '()
      (cons n (countdown (- n 1)))))

(define (insertR a b lat)
  (match lat
    ['() '()]
    [`(,x . ,rem) (if (eq? x a)
                      (cons x (cons b (insertR a b rem)))
                      (cons x (insertR a b rem)))]))

(define (remv-1st a lat)
  (match lat
    ['() '()]
    [`(,f . ,rem) (if (eq? a f) rem
                      (cons f (remv-1st a rem)))]))

(define (occurs-?s lat)
  (match lat
    ['() 0]
    [`(,f . ,rem) (if (eq? f '?)
                      (+ 1 (occurs-?s rem))
                      (occurs-?s rem))]))


(define list-index-ofv?
  (lambda (a lat)
    (cond
      [(eq? (car lat) a) 0]
      [else (+ 1 (list-index-ofv? a (cdr lat)))])))

(define (filter p lat)
  (match lat
    ['() '()]
    [`(,a . ,rem) (if (p a)
                      (cons a (filter p rem))
                      (filter p rem))]))

(define (myzip l1 l2)
  (if (null? l1) '()
      (cons `(,(car l1) . ,(car l2))
            (myzip (cdr l1) (cdr l2)))))

(define (mymap f lat)
  (if (null? lat)
      '()
      (cons (f (car lat)) (mymap f (cdr lat)))))

(define (append l1 l2)
  (cond
    [(null? l1) l2]
    [else (cons (car l1) (append (cdr l1) l2))]))

(define (reverse lat)
  (letrec ([R (lambda (l res)
                (if (null? l) res
                    (R (cdr l) (cons (car l) res))))])
    (R lat '())))

(define (fact n)
  (if (= n 0) 1
      (* n (fact (- n 1)))))

(define (member-?* l)
  (cond
    [(null? l) #t]
    [(list? (car l)) #f]
    [else (member-?* (cdr l))]))

(define (fib n)
  (letrec
      ([R (lambda (n f s)
            (if (= n 0)
                f
                (R (- n 1) s (+ f s))))])
    (R n 0 1)))

(define (cons-cell-count l)
  (match l
    [`(,ar . ,dr) (+ 1 (cons-cell-count ar)
                     (cons-cell-count dr))]
    [x 0]))

(define (natural->binary n)
  (letrec ([R (lambda (n res)
                (if (= n 0)
                    res
                    (R (quotient n 2) (cons (remainder n 2) res))))])
    (reverse (R n '()))))

(define (minus a b)
  (if (= b 0) a
      (minus (- a 1) (- b 1))))

(define (div a b)
  (letrec ([R (lambda (n res)
                (if (= n 0) res
                    (R (- n b) (+ 1 res))))])
    (R a 0)))

(define (append-map f lat)
  (cond
    [(null? lat) '()]
    [else (append (f (car lat)) (append-map f (cdr lat)))]))

(define (memv a lat)
  (cond
    [(null? lat) #f]
    [(eq? a (car lat)) lat]
    [else (memv a (cdr lat))]))

(define (set-difference l1 l2)
  (cond
    [(null? l1) '()]
    [(memv (car l1) l2) (set-difference (cdr l1) l2)]
    [else (cons (car l1) (set-difference (cdr l1) l2))]))


(define (powerset lat)
  (cond
    [(null? lat) '(())]
    [else (let ([dres (powerset (cdr lat))])
           (append (map (lambda (l) (cons (car lat) l))
                        dres) dres))]))

(define (cartesian-product llat)
  (letrec ([l1 (car llat)]
           [l2 (cadr llat)]
           [product (lambda (a lat)
                      (map (lambda (d) `(,a ,d)) lat))]
           [R (lambda (l1)
                (cond
                  [(null? l1) '()]
                  [else (append (product (car l1) l2) (R (cdr l1)))]))])
    (R l1)))

(define (zip l1 l2)
  (cond
    [(null? l1) '()]
    [(null? l2) '()]
    [else (cons (cons (car l1)
                      (car l2))
                (zip (cdr l1) (cdr l2)))]))

#;(define (insertR a b lat)
  (match lat
    ['() '()]
    [`(,x . ,rem) (if (eq? x a)
                      (cons x (cons b (insertR a b rem)))
                      (cons x (insertR a b rem)))]))

(define (insertR-fr a b lat)
  (foldr (lambda (x l)
            (if (eq? x a)
                (cons x (cons b l))
                (cons x l))) '() lat))

#;(filter-fr even? '(1 2 3 4 5 6))

(define (filter-fr pred lat)
  (foldr (lambda (ar dr)
           (if (pred ar)
               (cons ar dr)
               dr)) '() lat))

#;(map-fr add1 '(1 2 3 4))

(define (map-fr f lat)
  (foldr (lambda (ar dr)
           (cons (f ar) dr)) '() lat))

#;(define (append l1 l2)
  (cond
    [(null? l1) l2]
    [else (cons (car l1) (append (cdr l1) l2))]))

(define (append-fr l1 l2)
  (foldr cons l2 l1))

(define (reverse-fr l)
  (foldr (lambda (ar dr)
           (append dr `(,ar))) '() l))

#;(define (append-map f lat)
  (cond
    [(null? lat) '()]
    [else (append (f (car lat)) (append-map f (cdr lat)))]))

(define (append-map-fr f lat)
  (foldr (lambda (ar dres)
           (append (f ar) dres)) '() lat))

#;(define (set-difference l1 l2)
  (cond
    [(null? l1) '()]
    [(memv (car l1) l2) (set-difference (cdr l1) l2)]
    [else (cons (car l1) (set-difference (cdr l1) l2))]))

(define (set-difference-fr l1 l2)
  (foldr (lambda (ar dres)
           (if (memv ar l2) dres
               (cons ar dres))) '() l1))

#;(define (powerset lat)
  (cond
    [(null? lat) '(())]
    [else (let ([dres (powerset (cdr lat))])
           (append (map (lambda (l) (cons (car lat) l))
                        dres) dres))]))

(define (powerset-fr lat)
  (foldr (lambda (ar dres)
           (append (map (lambda (l) (cons ar l)) dres) dres)) '(()) lat))

#;(define (cartesian-product llat)
  (letrec ([l1 (car llat)]
           [l2 (cadr llat)]
           [product (lambda (a lat)
                      (map (lambda (d) `(,a ,d)) lat))]
           [R (lambda (l1)
                (cond
                  [(null? l1) '()]
                  [else (append (product (car l1) l2) (R (cdr l1)))]))])
    (R l1)))

(define (cartesian-product-fr llat)
  (letrec ([l1 (car llat)]
           [l2 (cadr llat)]
           [product (lambda (a lat)
                      (map (lambda (d) `(,a ,d)) lat))]
           [R (lambda (l1)
                (foldr (lambda (ar dres)
                         (append (product ar l2) dres)) '() l1))])
    (R l1)))


(define (binary->natural lnum)
  (if (null? lnum)
      0
      (+ (car lnum) (* 2 (binary->natural (cdr lnum))))))

(define (binary->natural-fr lnum)
  (foldr (lambda (ar dres)
           (+  ar (* 2 dres))) 0 lnum))

(define collatz
  (letrec
      ([odd-case
        (lambda (recur)
          (lambda (x)
            (cond 
              ((and (positive? x) (odd? x)) (collatz (add1 (* x 3)))) 
              (else (recur x)))))]
       [even-case
        (lambda (recur)
          (lambda (x)
            (cond 
              ((and (positive? x) (even? x)) (collatz (/ x 2))) 
              (else (recur x)))))]
       [one-case
        (lambda (recur)
          (lambda (x)
            (cond
              ((zero? (sub1 x)) 1)
              (else (recur x)))))]
       [base
        (lambda (x)
          (error 'error "Invalid value ~s~n" x))])
    (one-case (odd-case (even-case base)));; this should be a single line, without lambda
    ))
