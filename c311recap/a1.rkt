#lang racket
;;A1
(define (countdown n)
  (if (= n -1)
      '()
      (cons n (countdown (- n 1)))))

(countdown 5)

(define (insertR a b lat)
  (match lat
    ['() '()]
    [`(,x . ,rem) (if (eq? x a)
                      (cons x (cons b (insertR a b rem)))
                      (cons x (insertR a b rem)))]))

(insertR 'x 'y '(x z z x y x))

(define (remv-1st a lat)
  (match lat
    ['() '()]
    [`(,f . ,rem) (if (eq? a f) rem
                      (cons f (remv-1st a rem)))]))

(remv-1st 'y '(x y z y x))

(define (occurs-?s lat)
  (match lat
    ['() 0]
    [`(,f . ,rem) (if (eq? f '?)
                      (+ 1 (occurs-?s rem))
                      (occurs-?s rem))]))

(occurs-?s '(? y z ? ?))

(define (myfilter p lat)
  (match lat
    ['() '()]
    [`(,a . ,rem) (if (p a)
                      (cons a (myfilter p rem))
                      (myfilter p rem))]))
(myfilter even? '(1 2 3 4 5 6))

(define (myzip l1 l2)
  (if (null? l1) '()
      (cons `(,(car l1) . ,(car l2))
            (myzip (cdr l1) (cdr l2)))))

(myzip '(1 2 3) '(a b c))

(define (mymap f lat)
  (if (null? lat)
      '()
      (cons (f (car lat)) (mymap f (cdr lat)))))

(mymap (lambda (x) (+ x 1)) '(1 2 3 4))

(define (myappend l1 l2)
  `(,@l1 ,@l2))

(define (myreverse lat)
  (letrec ([R (lambda (l res)
                (if (null? l) res
                    (R (cdr l) (cons (car l) res))))])
    (R lat '())))

(myreverse '(1 2 3))

(define (fact n)
  (if (= n 0) 1
      (* n (fact (- n 1)))))

(fact 5)

(define (member-?* l)
  (cond
    [(null? l) #t]
    [(list? (car l)) #f]
    [else (member-?* (cdr l))]))

(member-?* '(1 2 3))

(define (fib n)
  (letrec
      ([R (lambda (n f s)
            (if (= n 0)
                f
                (R (- n 1) s (+ f s))))])
    (R n 0 1)))

(fib 7)

(define (cons-cell-count l)
  (match l
    [`(,ar . ,dr) (+ 1 (cons-cell-count ar)
                     (cons-cell-count dr))]
    [x 0]))

(cons-cell-count '((a b . c) 3 . 4))

(define (binary->natural lnum)
  (letrec
      ([R (lambda (l base res)
            (if (null? l)
                res
                (R (cdr l) (* 2 base) (+ res (* base (car l))))))])
    (R lnum 1 0)))

(binary->natural '(1 0 1 0 1))

(define (natural->binary n)
  (letrec ([R (lambda (n res)
                (if (= n 0)
                    res
                    (R (quotient n 2) (cons (remainder n 2) res))))])
    (reverse (R n '()))))


(natural->binary 4)

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
