#lang racket
;;A2
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

(define (unique-vars s)
  (match s
    [v #:when (symbol? v) `(,v)]
    [`(lambda (,x) ,body) (unique-vars body)]
    [`(,rator ,rand) (union (unique-vars rator) (unique-vars rand))]))

(define (extend x pred)
  (lambda (y) (or (eq? x y)
                  (pred y))))


;    [v #:when (symbol? v)]
;    [`(lambda (,x) ,body)]
;    [`(,rator ,rand)]

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

(define (bound s)
  (match s
    [v #:when (symbol? v) '()]
    [`(lambda (,x) ,body) (union `(,x) (bound body))]
    [`(,rator ,rand) (union (bound rator)
                            (bound rand))]))

(define (walk-symbol sym assl)
  (letrec ([R (lambda (l)                
   (match l
     ['() sym]
     [`(,a . ,rem) (if (eq? (car a) sym)
                       (if (symbol? (cdr a))
                           (walk-symbol (cdr a) assl)
                           (cdr a))
                       (R rem))]))])
    (R assl)))

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

(define (var-occurs? x term)
  (match term
    [s #:when (symbol? s) (eqv? x s)]
    [`(lambda (,s) ,body) (if (eqv? s x) #f
                              (var-occurs? x body))]
    [`(,rator ,rand) (or (var-occurs? x rator)
                         (var-occurs? x rand))]))

(define (var-occurs-free? x term)
  (match term
    [s #:when (symbol? s) (eqv? x s)]
    [`(lambda (,s) ,body) (if (eqv? s x) #f
                              (var-occurs-free? x body))]
    [`(,rator ,rand) (or (var-occurs-free? x rator)
                         (var-occurs-free? x rand))]))

(define (rember x lat)
  (cond
    [(null? lat) '()]
    [(eqv? x (car lat)) (cdr lat)]
    [else (cons (car lat) (rember x (cdr lat)))]))

(define (unique-free-vars term)  
  (match term
    [s #:when (symbol? s) `(,s)]
    [`(lambda (,s) ,body) (rember s (unique-free-vars body))]
    [`(,rator ,rand) (union (unique-free-vars rator)
                            (unique-free-vars rand))]))

(define (var-occurs-bound? x term)
  (match term
    [s #:when (symbol? s) #f]
    [`(lambda (,s) ,body) (or (and (eqv? s x) (var-occurs? s body))
                              (var-occurs-bound? x body))]
    [`(,rator ,rand) (or (var-occurs-bound? x rator)
                            (var-occurs-bound? x rand))]))

(define (unique-bound-vars term)  
  (match term
    [s #:when (symbol? s) '()]
    [`(lambda (,s) ,body) (if (var-occurs? s body)
                              (union `(,s) (unique-bound-vars body))
                              (unique-bound-vars body))]
    [`(,rator ,rand) (union (unique-bound-vars rator)
                            (unique-bound-vars rand))]))

(define (walk-symbol-update x assl)
  (letrec ([R (lambda (x acc)
                (let ([xres (assv x assl)])
                 (if xres
                     (let ([bv (unbox (cdr xres))])
                      (if (symbol? bv)
                          (R (unbox (cdr xres)) (cons (cdr xres) acc))
                          (begin (for-each (lambda (b) (set-box! b bv)) acc) bv)))
                     x)))])
    (R x '())))

(define (var-occurs-both? x term)
  (match term
    [s #:when (symbol? s) (values (eqv? x s) #f)]
    [`(lambda (,s) ,body) (let-values([(fb bb) (var-occurs-both? x body)])
                            (values (if (eqv? s x) #f fb)
                                    (or (and (eqv? s x) fb)
                                        bb)))]
    [`(,rator ,rand) (let-values ([(torf torb) (var-occurs-both? x rator)]
                                  [(randf randb) (var-occurs-both? x rand)])
                       (values (or torf randf)
                               (or torb randb)))]))
