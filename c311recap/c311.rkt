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

;;A2
(println "-------------A2------------------")
(define (list-ref ls n)
  (letrec ([nth-cdr (lambda (n)
                      (if (= n 0)
                          ls
                          (cdr (nth-cdr (- n 1)))))])
    (car (nth-cdr n))))

(define (lambda->lumbda x)
  (match x
    [a #:when (symbol? a) a]
    [`(lambda (,x) ,body) `(lumbda (,x) ,(lambda->lumbda body))]
    [`(,rator ,rand) `(,(lambda->lumbda rator)
                       ,(lambda->lumbda rand))]))

(lambda->lumbda '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a))))))

(define (vars s)
  (match s
    [v #:when (symbol? v) `(,v)]
    [`(lambda (,x) ,body) (vars body)]
    [`(,rator ,rand) (append (vars rator) (vars rand))]))


(define (union l1 l2)
  (match l1
    ['() l2]
    [`(,ar . ,dr) (if (memv ar l2)
                      (union dr l2)
                      (cons ar (union dr l2)))]))

(union '(x y) '(x z))


(define (unique-vars s)
  (match s
    [v #:when (symbol? v) `(,v)]
    [`(lambda (,x) ,body) (unique-vars body)]
    [`(,rator ,rand) (union (unique-vars rator) (unique-vars rand))]))

(unique-vars '((lambda (a) (a b)) ((lambda (c) (a c)) (b a))))

(define (extend x pred)
  (lambda (y) (or (eq? x y)
                  (pred y))))


;    [v #:when (symbol? v)]
;    [`(lambda (,x) ,body)]
;    [`(,rator ,rand)]

(define (free? var s)
  (letrec ([R (lambda (bound? sub)
                (match sub
                  [v #:when (symbol? v) (if (eq? var v)
                                            (not (bound? v))
                                            #f)]
                  [`(lambda (,x) ,body) (R (extend x bound?) body)]
                  [`(,rator ,rand) (or (R bound? rator)
                                       (R bound? rand))]))])
    (R (lambda (x) #f) s)))

(define (bound? var s)
  (letrec
      ([R (lambda (met? sub)
            (match sub
              [v #:when (symbol? v) (met? var)]
              [`(lambda (,x) ,body) (R (extend x met?) body)]
              [`(,rator ,rand) (or (R met? rator)
                                   (R met? rand))]))])
    (R (lambda (x) #f) s)))

(bound? 'z '(lambda (y) (lambda (z) (y z))))

(define (free s)
  (letrec ([R (lambda (bound? sub)
                (match sub
                  [v #:when (symbol? v) (if (not (bound? v))
                                            `(,v)
                                            '())]
                  [`(lambda (,x) ,body) (R (extend x bound?) body)]
                  [`(,rator ,rand) (union (R bound? rator)
                                          (R bound? rand))]))])
    (R (lambda (x) #f) s)))

(free '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c)))))))

(define (bound s)
  (match s
    [v #:when (symbol? v) '()]
    [`(lambda (,x) ,body) (union `(,x) (bound body))]
    [`(,rator ,rand) (union (bound rator)
                            (bound rand))]))

(bound '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c)))))))

(define (walk-symbol sym assl)
  (match assl
    ['() sym]
    [`((,ar . ,dr) . ,rem) (if (eq? ar sym)
                               dr
                               (walk-symbol sym rem))]))

(walk-symbol 'b '((a . 5) (b . ((c . a))) (c . a)))

(define (lex s acc)
  (letrec
      ([R (lambda (s acc depth)
            (match s
              [v #:when (symbol? v)
                 (let ([dep (walk-symbol s acc)])
                   (if (eq? v dep)
                       `(free-var ,v)
                       `(var ,(- depth dep 1))))]
              [`(lambda (,x) ,body)
               `(lambda ,(R body (cons `(,x . ,depth) acc) (+ depth 1)))]
              [`(,rator ,rand)
               `(,(R rator acc depth)
                 ,(R rand acc depth))]))])
    (R s acc 0)))

(lex 'x '())
(lex '(lambda (x) (x y)) '())
(lex '((lambda (x) (x y)) (lambda (c) (lambda (d) (e c)))) '())
(lex '(lambda (a)
        (lambda (b)
          (lambda (c)
            (lambda (a)
              (lambda (b)
                (lambda (d)
                  (lambda (a)
                    (lambda (e)
                      (((((a b) c) d) e) f))))))))) '())

(lex '((lambda (a)
         (lambda (b)
           (lambda (c)
             (((((a b) c) w) x) y))))
       (lambda (w)
         (lambda (x)
           (lambda (y)
             (((((a b) c) w) x) y))))) '())
