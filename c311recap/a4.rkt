#lang racket

;;Part I

(define (lex s acc)
  (match s
    [c #:when (number? c) `(const ,c)]
    [b #:when (boolean? b) `(const ,b)]
    [`(zero? ,zt) `(zero? ,(lex zt acc))]
    [`(sub1 ,st) `(sub1 ,(lex st acc))]
    [`(* ,n1 ,n2) `(* ,(lex n1 acc) ,(lex n2 acc))]
    [`(if ,test ,conseq ,alt) `(if ,(lex test acc)
                                   ,(lex conseq acc)
                                   ,(lex alt acc))]
    [`(let ([,n ,v]) ,body) #:when (symbol? n) `(let ,(lex v acc) ,(lex body (cons n acc)))]
    [v #:when (symbol? v) `(var ,(index-of acc v))]
    [`(lambda (,x) ,body) #:when (symbol? x)
                          `(lambda ,(lex body (cons x acc)))]
    [`(,rator ,rand) `(,(lex rator acc) ,(lex rand acc))]))


;;Part II

(struct Closure (x body env) #:transparent)
(define (closure-ds x body env)
  (Closure x body env))
(define (apply-closure-ds clo xv)
  (match clo
    [(Closure cx cbody cenv) (value-of-ds cbody (extend-env cenv cx xv))]
    [else (error 'apply-env "applying non-closure" clo xv)]))


(define (empty-env-fn) (lambda (x) (error 'value-of "unbound variable ~s" x)))
(define (extend-env-fn env n v)
  (lambda (x)
    (if (eqv? x n) v
        (env x))))
(define (apply-env-fn env n)
  (env n))

(define extend-env extend-env-fn)
(define apply-env apply-env-fn)
(define empty-env empty-env-fn)

(define (value-of-ds term env)
  (match term
    [n #:when (number? n) n]
    [b #:when (boolean? b) b]
    [`(zero? ,zt) (zero? (value-of-ds zt env))]
    [`(sub1 ,st) (sub1 (value-of-ds st env))]
    [`(* ,n1 ,n2) (* (value-of-ds n1 env) (value-of-ds n2 env))]
    [`(if ,test ,conseq ,alt) (if (value-of-ds test env)
                                  (value-of-ds conseq env)
                                  (value-of-ds alt env))]
    [var #:when (symbol? var) (apply-env env var)]
    [`(lambda (,x) ,body) #:when (symbol? x) (closure-ds x body env)]
    [`(,rator ,rand) (let ([ratorv (value-of-ds rator env)]
                           [randv (value-of-ds rand env)])
                       (apply-closure-ds ratorv randv))]
    [`(let ([,n ,v]) ,body) #:when (symbol? n)
                            (value-of-ds body (extend-env env n (value-of-ds v env)))]))

(define (apply-closure-fn clo xv)
  (clo xv))

(define (closure-fn x body env)
  (lambda (xv)
     (value-of-fn body (extend-env env x xv))))

(define (value-of-fn term env)
  (match term
    [n #:when (number? n) n]
    [b #:when (boolean? b) b]
    [`(zero? ,zt) (zero? (value-of-fn zt env))]
    [`(sub1 ,st) (sub1 (value-of-fn st env))]
    [`(* ,n1 ,n2) (* (value-of-fn n1 env) (value-of-fn n2 env))]
    [`(if ,test ,conseq ,alt) (if (value-of-fn test env)
                                  (value-of-fn conseq env)
                                  (value-of-fn alt env))]
    [var #:when (symbol? var) (apply-env env var)]
    [`(lambda (,x) ,body) #:when (symbol? x) (closure-fn x body env)]
    [`(,rator ,rand) (let ([ratorv (value-of-fn rator env)]
                           [randv (value-of-fn rand env)])
                       (apply-closure-fn ratorv randv))]
    [`(let ([,n ,v]) ,body) #:when (symbol? n)
                            (value-of-fn body (extend-env env n (value-of-fn v env)))]))

(define (apply-closure-danamic clo xv env)
  (clo xv env))

(define (closure-danamic x body)
  (lambda (xv env)
     (value-of-dynamic body (extend-env env x xv))))

(define (value-of-dynamic term env)
  (match term
    ;;how to interp (quote v) ? ,let current evaluator evaluate it.
    ;; (eval v)
    [`(quote ,v) v]
    [n #:when (number? n) n]
    [b #:when (boolean? b) b]
    [`(zero? ,zt) (zero? (value-of-dynamic zt env))]
    [`(sub1 ,st) (sub1 (value-of-dynamic st env))]
    [`(* ,n1 ,n2) (* (value-of-dynamic n1 env) (value-of-dynamic n2 env))]
    [`(if ,test ,conseq ,alt) (if (value-of-dynamic test env)
                                  (value-of-dynamic conseq env)
                                  (value-of-dynamic alt env))]
    [`(null? ,nt) (null? (value-of-dynamic nt env))]
    [`(car ,at) (car (value-of-dynamic at env))]
    [`(cdr ,dt) (cdr (value-of-dynamic dt env))]
    [`(cons ,a ,d) (cons (value-of-dynamic a env) (value-of-dynamic d env))]
    [var #:when (symbol? var) (apply-env env var)]
    [`(lambda (,x) ,body) #:when (symbol? x) (closure-danamic x body)]
    [`(,rator ,rand) (let ([ratorv (value-of-dynamic rator env)]
                           [randv (value-of-dynamic rand env)])
                       (apply-closure-danamic ratorv randv env))]
    [`(let ([,n ,v]) ,body) #:when (symbol? n)
                            (value-of-dynamic body (extend-env env n (value-of-dynamic v env)))]))








;Closure

(struct Closure-ds-ri (x body env interp) #:transparent)

(define (closure-ds-ri x body env interp)
  (Closure-ds-ri x body env interp))
(define (apply-closure-ds-ri clo xv ext)
  (match clo
    [(Closure-ds-ri cx cbody cenv interp) (interp cbody (ext cenv cx xv))]
    [else (error 'apply-env "applying non-closure" clo xv)]))



(define (closure-fn-ri x body env interp)
  (lambda (xv ext)
    (interp body (ext env x xv))))

(define (apply-closure-fn-ri clo xv ext)
  (clo xv ext))

;Env

(define (empty-env-ds) '())
(define (extend-env-ds env n v) (cons `(,n . ,v) env))
(define (apply-env-ds env n)
  (let ([res (assv n env)])
    (if res
        (cdr res)
        (error 'value-of "unbound variable ~s" n))))

(define (extend-env-fn-ri env n v)
  (lambda (x)
    (if (eqv? x n) v
        (env x))))
(define (apply-env-fn-ri env n)
  (env n))

(define (value-of-ri empty-env extend-env apply-env closure-ri apply-closure-ri)
  (letrec ([R (lambda (term env)                 
                (match term
                  [n #:when (number? n) n]
                  [b #:when (boolean? b) b]
                  [`(zero? ,zt) (zero? (R zt env))]
                  [`(sub1 ,st) (sub1 (R st env))]
                  [`(* ,n1 ,n2) (* (R n1 env) (R env))]
                  [`(if ,test ,conseq ,alt) (if (R test env)
                                                (R conseq env)
                                                (R alt env))]
                  [var #:when (symbol? var) (apply-env env var)]
                  [`(lambda (,x) ,body) #:when (symbol? x) (closure-ri x body env R)]
                  [`(,rator ,rand) (let ([ratorv (R rator env)]
                                         [randv (R rand env)])
                                     (apply-closure-ri ratorv randv extend-env))]
                  [`(let ([,n ,v]) ,body) #:when (symbol? n)
                                          (R body (extend-env env n (R v env)))]))])
    (lambda (term)
      (R term (empty-env)))))
