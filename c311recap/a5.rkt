#lang racket


#|
reading https://cgi.soic.indiana.edu/~c311/lib/exe/fetch.php?media=call-by.pdf
|#
(define (empty-env)
  (lambda (x) (error "empty env met")))
(define (extend-env env k v)
  (lambda (a) (if (eq? a k) v (env a))))
(define (apply-env env k)
  (env k))


(define (val-of exp env)
  (match exp
    [l #:when (or (number? l) (boolean? l)) l]
    [x #:when (symbol? x) (apply-env env x)]
    [`(lambda (,x) ,body)
     (lambda (a)
       (val-of body (extend-env env x a)))]
    [ `(,rator ,x) #:when (symbol? x) ((val-of rator env) (apply-env env x))]
    [`(,rator ,rand) #:when (not (symbol? rand))
     ((val-of rator env) (val-of rand env))]))


(define (deepCopyBox b)
  (box (unbox b)))

(define (val-of-cbv exp env)
  (match exp
    [l #:when (or (number? l) (boolean? l)) l]
    [x #:when (symbol? x) (unbox (apply-env env x))]
    [`(set! ,x ,rhs) #:when (symbol? x)
     (let ([vrhs (val-of rhs env)])
       (set-box! (apply-env env x) vrhs))]
    [`(begin ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
    [`(lambda (,x) ,body)
     (lambda (a)
       (val-of-cbv body (extend-env env x a)))]
    #|
      to remove the scary prospect of passing around the same box
      - the same reference to memory - as the one in the environment,
      on symbol look-up in the rator/x line, we unbox and rebox the value, thus aquiring a new box.
    |#
    [ `(,rator ,x) #:when (symbol? x) ((val-of-cbv rator env) (box (unbox (apply-env env x))))]
    [`(,rator ,rand)  #:when (not (symbol? rand))
     ((val-of-cbv rator env) (box (val-of-cbv rand env)))]))

#|
These boxes, however, give us even more power than side effects:
they let us change the calling convention used for evaluation in our langauge.
|#
(define case1
 '((lambda (x) ((lambda (y) (begin (set! y 2) x)) x)) 5))


; call-by-value semantics: arguments are evaluated and the resulting value is passed into the function.
(val-of-cbv case1 (empty-env))



; call-by-reference semantics—instead of passing a value around, we will pass in a reference to the value. 
(define (val-of-cbr exp env)
  (match exp
    [l #:when (or (number? l) (boolean? l)) l]
    [x #:when (symbol? x) (unbox (apply-env env x))]
    [`(set! ,x ,rhs) #:when (symbol? x)
     (let ([vrhs (val-of rhs env)])
       (set-box! (apply-env env x) vrhs))]
    [`(begin ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
    [`(lambda (,x) ,body)
     (lambda (a)
       (val-of-cbr body (extend-env env x a)))]
    #|
    passing the same box 
    |#
    [ `(,rator ,x) #:when (symbol? x) ((val-of-cbr rator env) (apply-env env x))]
    [`(,rator ,rand) #:when (not (symbol? rand))
     ((val-of-cbr rator env) (box (val-of-cbr rand env)))]))


(val-of-cbr case1 (empty-env))



#|
For our next step, we can dispense with the concept of side effects
- our next two semantics shifts deal with when things are evaluated
|#

(define omegalist
  '((lambda (x) (x x))
    (lambda (x) (x x))))

(define lazyget5
  `((lambda (f)
      (if (zero? 0)
          5
          f)) ,omegalist))

#|
This time, we will suspend the entire computation to be performed until its result is asked for.
And we can accomplish this using thunks.
|#

; A thunk is a function that takes no arguments

(define (five) (+ 2 3))
;or
(define five2
  (lambda () (+ 2 3)))

(define (omega)
  ((lambda (x) (x x))
   (lambda (x) (x x))))


#|Call-By-Name
In call-by-name semantics, the evaluation itself is stored
and repeated every time the value is asked for.
`((x . (lambda () (value-of 2 '()))))
`((x . (box (lambda () (value-of 2 '()))))) here
this computation is not performed until
 the value of x is looked up in the environment
|#

(define (val-of-cbname exp env)
  (match exp
    [l #:when (or (number? l) (boolean? l)) l]
    
    [`(zero? ,b) (eq? (val-of-cbname b env) 0)]
    [`(sub1 ,n) (- (val-of-cbname n env) 1)]
    [`(* ,e1 ,e2) (* (val-of-cbname e1 env) (val-of-cbname e2 env))]
    [`(if ,pred ,conseq ,alt) (if (val-of-cbname pred env)
                                  (val-of-cbname conseq env)
                                  (val-of-cbname alt env))]
    [x #:when (symbol? x) ((unbox (apply-env env x)))]
    [`(lambda (,x) ,body)
     (lambda (a) (val-of-cbname body (extend-env env x a)))]
    [`(,rator ,x) #:when (symbol? x)
     ((val-of-cbname rator env) (apply-env env x))]
    [`(,rator ,rand) #:when (not (symbol? rand))
     ((val-of-cbname rator env)
      (box (lambda () (val-of-cbname rand env))))]))


#|Call-By-Need
In call-by-name, the computation is repeated
every time the variable is looked up.
In call-by-need,however,
the computation is performed the first time the variable is used
and the result is then stored back in its place.
|#

(define (unbox/need b)
  (let ([val ((unbox b))])
    (set-box! b (lambda () val))
    val))


(define (val-of-cbneed exp env)
  (match exp    
    [l #:when (or (number? l) (boolean? l)) l]
    [`(zero? ,b) (eq? (val-of-cbneed b env) 0)]
    [`(sub1 ,n) (- (val-of-cbneed n env) 1)]
    [`(* ,e1 ,e2) (* (val-of-cbneed e1 env) (val-of-cbneed e2 env))]
    [`(if ,pred ,conseq ,alt) (if (val-of-cbneed pred env)
                                  (val-of-cbneed conseq env)
                                  (val-of-cbneed alt env))]
    
    [x #:when (symbol? x) (unbox/need (apply-env env x))]
    [`(lambda (,x) ,body)
     (lambda (a) (val-of-cbneed body (extend-env env x a)))]
    [`(,rator ,x) #:when (symbol? x)
     ((val-of-cbneed rator env)
      (apply-env env x))]
    [`(,rator ,rand) #:when (not (symbol? rand))
      ((val-of-cbneed rator env)
       (box (lambda () (val-of-cbneed rand env))))]))

(val-of-cbname lazyget5 (empty-env))
(val-of-cbneed lazyget5 (empty-env))