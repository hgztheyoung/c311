#lang racket

(define (lex s)
  (define (walk-symbol sym assl)
    (match assl
      ['() sym]
      [`((,ar . ,dr) . ,rem) (if (eq? ar sym)
                                 dr
                                 (walk-symbol sym rem))]))
  (letrec
      ([R (lambda (s acc depth)
            (match s
              [c #:when (or (boolean? c) (number? c)) c]
              [`(sub1 ,body) `(sub1 ,(R body acc depth))]
              [`(zero? ,body) `(zero? ,(R body acc depth))]
              [`(* ,n1 ,n2) `(* ,(R n1 acc depth) ,(R n2 acc depth))]
              [`(if ,t ,c ,a) `(if ,(R t acc depth)
                                   ,(R c acc depth)
                                   ,(R a acc depth))]
              [v #:when (symbol? v)
                 (let ([dep (walk-symbol s acc)])
                   (if (eq? v dep)
                       `(free-var ,v)
                       `(var ,(- depth dep 1))))]
              [`(lambda (,x) ,body)
               `(lambda ,(R body (cons `(,x . ,depth) acc) (+ depth 1)))]
              [`(let ([,k ,n]) ,body)
               (R `((lambda (,k) ,body) ,n) acc depth)]
              [`(,rator ,rand)
               `(,(R rator acc depth)
                 ,(R rand acc depth))]
              [x x]))])
    (R s '() 0)))

(define (empty-env)
  (vector))

(define (apply-env-lex env num)
  (vector-ref env num))

(define (extend-env-lex a env)
  (vector-append (vector a) env))

(define value-of-lex
  (lambda(exp env)
    (match exp
      [c #:when (or (boolean? c) (number? c)) c]
      [`(sub1 ,body) (sub1 (value-of-lex body env))]
      [`(zero? ,body) (zero? (value-of-lex body env))]
      [`(* ,n1 ,n2) (* (value-of-lex n1 env) (value-of-lex n2 env))]
      [`(if ,t ,c ,a) (if (value-of-lex t env) (value-of-lex c env) (value-of-lex
                                                                     a env))]
      [`(var ,num) (apply-env-lex env num)]
      [`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env)))]
      [`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))])))

(value-of-lex '((lambda (var 0)) 5) (empty-env))

(define cases
  (list
   '((lambda (x) (if (zero? x)
                     12
                     47))
     0)
   '(let ([y (* 3 4)])
      ((lambda (x) (* x y)) (sub1 6)))
   '(let ([x (* 2 3)])
      (let ([y (sub1 x)])
        (* x y)))

   '(let ([x (* 2 3)])
      (let ([x (sub1 x)])
        (* x x)))
   '(let ((! (lambda (x) (* x x))))
      (let ((! (lambda (n)
                 (if (zero? n) 1 (* n (! (sub1 n)))))))
        (! 5)))

   '(((lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
      (lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
     5)))


(map (lambda (s) (lex s)) cases)
(map (lambda (s) (value-of-lex (lex s) (empty-env))) cases)

'(((lambda (lambda (if (zero? (var 0)) 1 (* (var 0) (((var 1) (var 1)) (sub1 (var 0)))))))
   (lambda (lambda (if (zero? (var 0)) 1 (* (var 0) (((var 1) (var 1)) (sub1 (var 0))))))))
  5)