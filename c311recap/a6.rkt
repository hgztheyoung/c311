#lang racket


#|
First rule: whenever we see a lambda in the code we want to CPS, we
have to add an argument, and then process the body:


Second rule: "Don't sweat the small stuff!"
|#
(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only 
            (error 'empty-k "You can only invoke the empty continuation once")
            (begin (set! once-only #t) v))))))


(define last-non-zero
  (lambda (ls)
    (let/cc k
     (letrec
         ((lnz
           (lambda (ls)
             (match ls
               ['() '()]
               [`(,ar . ,dr) (if (= ar 0) (k (last-non-zero dr))
                                 `(,ar . ,(lnz dr)))]))))
       (lnz ls)))))

(last-non-zero '(1 0 2 3 0 4 5))

(last-non-zero '(1 0 2 3 4 5))

(last-non-zero '(1 2 3 4 5))

(define mult/cc
  (lambda (n*)
    (let/cc k
     (letrec
         ([m/cc
           (lambda (n*)
             (cond
               ((null? n*) 1)
               ((zero? (car n*)) (k 0))
               (else (* (car n*) (m/cc (cdr n*))))))])
       (m/cc n*)))))

(mult/cc `(1 2 3))
(mult/cc `(1 0 2 3))

;--------- part 2 cps ----------

(define times
  (lambda (ls)
    (cond
      [(null? ls) 1]
      [(zero? (car ls)) 0]
      [else (* (car ls) (times (cdr ls)))])))


(define (times-cps ls k)
  (cond
    [(null? ls) (k 1)]
    [(zero? (car ls)) (k 0)]
    [else (times-cps (cdr ls) (lambda (dres)
                                (k (* (car ls) dres))))]))

(times-cps '(1 2 3 4) (empty-k))


(define (times-cps-shortcut ls k)
  (cond
    [(null? ls) (k 1)]
    [(zero? (car ls)) 0]
    [else (times-cps (cdr ls) (lambda (dres)
                                (k (* (car ls) dres))))]))

(define (plus m)
  (lambda (n)
    (+ m n)))

(define (plus-cps m k)
  (k (lambda (n k1)
     (k1 (+ m n)))))

((plus 2) 3)
((plus-cps 2 (empty-k)) 3 (empty-k))

((plus ((plus 2) 3)) 5)
((plus-cps ((plus-cps 2 (empty-k)) 3 (empty-k)) (empty-k)) 5 (empty-k))

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
      [(pair? (car ls)) (count-syms*-cps (car ls)
                                         (lambda (ares)
                                           (count-syms*-cps (cdr ls) (lambda (dres)
                                                                       (k (+ ares dres))))))]
      [(symbol? (car ls)) (count-syms*-cps (cdr ls) (lambda (dres)
                                                      (k (add1 dres))))]
      [else (count-syms*-cps (cdr ls) k)])))


(count-syms* '(a 1 b 2 c 3))
(count-syms* '((a 1) (b 2) (c 3)))
(count-syms* '(1 (b (3 (d (5 e) 7) (g)) 9) ((h))))

(count-syms*-cps '(a 1 b 2 c 3) (empty-k))
(count-syms*-cps '((a 1) (b 2) (c 3)) (empty-k))
(count-syms*-cps '(1 (b (3 (d (5 e) 7) (g)) 9) ((h))) (empty-k))



(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls)       
       (cons-cell-count-cps (car ls) (lambda (ares)
                                    (cons-cell-count-cps (cdr ls) (lambda (dres)
                                                                (k (add1 (+ ares dres)))))))]
      [else (k 0)])))


(cons-cell-count-cps `(1 ((2) (1) ((())))) (empty-k))


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
      [(zero? n) (ack-cps (sub1 m) 1 k)]
      [else (ack-cps m (sub1 n) (lambda (mn)
                              (ack-cps (sub1 m) mn (lambda (res)
                                                 (k res)))))])))


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
    ((lambda (fib k)
       (fib fib n k))
     (lambda (fib n)
       (cond
         [(zero? n) (k 0)]
         [(= 1 n) (k 1)]
         [else (fib fib (sub1 n) (lambda (r1)
                                   (fib fib (sub1 (sub1 n)) (lambda (r2)
                                                              (k (+ r1 r2))))))])) k)))


(define unfold
  (lambda (p f g seed)
    ((lambda (h)
       ((h h) seed '()))
     (lambda (h)
       (lambda (seed ans)
         (if (p seed)
             ans
             ((h h) (g seed) (cons (f seed) ans))))))))


(define unfold-cps
  (lambda (p f g seed k1)
    ((lambda (h k2)
       (h h (lambda (hh)
              (hh seed '() k2))))
     (lambda (h k3)
       (k3 (lambda (seed ans k4)
             (p seed (lambda (pseed)
                       (if pseed
                           (k4 ans)
                           (h h (lambda (hh)
                                  (g seed (lambda (gseed)
                                            (f seed (lambda (fseed)
                                                      (hh gseed (cons fseed ans) k4)))))))))))))
     k1)))



(define null?-cps
  (lambda (ls k)
    (k (null? ls))))

(define car-cps
  (lambda (pr k)
    (k (car pr))))


(define cdr-cps
  (lambda (pr k)
    (k (cdr pr))))

(unfold-cps null?-cps car-cps cdr-cps '(a b c d e) (empty-k))


(define empty-s
  (lambda ()
    '()))
(define extend-s
  (lambda (x v s)
    (cons `(,x . ,v) s)))

(define (find name env)
  (let ([res (assv name env)])
    (if res
        (cdr res)
        name)))


(define walk
  (lambda (v ls)
    (cond
      [(symbol? v)
       (let ((p (assq v ls)))
         (cond
           [p (walk (cdr p) ls)]
           [else v]))]
      [else v])))

(define walk-cps
  (lambda (v ls k)
    (cond
      [(symbol? v)
       (let ((p (assq v ls)))
         (cond
           [p (walk-cps (cdr p) ls k)]
           [else (k v)]))]
      [else (k v)])))

(define unify
  (lambda (v w s)
    (let ([v (walk v s)])
      (let ([w (walk w s)])
        (cond
          [(eqv? v w) s]
          [(symbol? v) (extend-s v w s)]
          [(symbol? w) (extend-s w v s)]
          [(and (pair? v) (pair? w))
           (let ((s (unify (car v) (car w) s)))
             (cond
               [s (unify (cdr v) (cdr w) s)]
               [else #f]))]
          [(equal? v w) s]
          [else #f])))))

(define unify-cps
  (lambda (v w s k1)
    (walk-cps v s (lambda (v)
       (walk-cps w s (lambda (w)
          (cond
            [(eqv? v w) (k1 s)]
            [(symbol? v) (k1 (extend-s v w s))]
            [(symbol? w) (k1 (extend-s w v s))]
            [(and (pair? v) (pair? w))
             (unify-cps (car v) (car w) s (lambda (s)
                (cond
                  [s (unify-cps (cdr v) (cdr w) s k1)]
                  [else (k1 #f)])))]
            [(equal? v w) (k1 s)]
            [else (k1 #f)])))))))


(unify-cps '(x y z) '(5 x y) (empty-s) (empty-k))

(define M
  (lambda (f)
    (lambda (ls)
      (cond
        ((null? ls) '())
        (else (cons (f (car ls)) ((M f) (cdr ls))))))))
(define use-of-M
  ((M (lambda (n) (add1 n))) '(1 2 3 4 5)))

use-of-M

(define M-cps
  (lambda (f k1)
    (k1 (lambda (ls k2)
       (cond
         [(null? ls) (k2 '())]
         [else (f (car ls) (lambda (fcarls)
                   (M-cps f (lambda (Mf)
                      (Mf (cdr ls) (lambda (Mfcdrls)
                          (k2 (cons fcarls Mfcdrls))))))))])))))

(define use-of-M-cps
  ((M-cps (lambda (n k) (k (add1 n))) (empty-k)) '(1 2 3 4 5)
                                                 (empty-k)))

use-of-M-cps

(define strange
  (lambda (x)
    ((lambda (g) (lambda (x) (g g)))
     (lambda (g) (lambda (x) (g g))))))

(define use-of-strange
  (let ([strange^ (((strange 5) 6) 7)])
    (((strange^ 8) 9) 10)))



(define strange-cps
  (lambda (x k1)
    ((lambda (g k2) (k2 (lambda (x k4) (g g (lambda (gg)
                                              (k4 gg))))))
     (lambda (g k3) (k3 (lambda (x k5) (g g (lambda (gg)
                                              (k5 gg))))))
     k1)))


(define why
  (lambda (f)
    ((lambda (g)
       (f (lambda (x) ((g g) x))))
     (lambda (g)
       (f (lambda (x) ((g g) x)))))))


(define why-cps
  (lambda (f k1)
    ((lambda (g k2)
       (f (lambda (x k3)
            (g g (lambda (gg)
                   (gg x k3)))) k2))
     (lambda (g k4)
       (f (lambda (x k5)
            (g g (lambda (gg)
                   (gg x k5)))) k4))
     k1)))



(define why-cps-cps
  (lambda (f k1 k6)
    ((lambda (g k2 k7)
       (f (lambda (x k3 k8)
            (g g (lambda (gg k9)
                   (gg x k3 k9)) k8)) k2 k7))
     (lambda (g k4 k10)
       (f (lambda (x k5 k11)
            (g g (lambda (gg k12)
                   (gg x k5 k12)) k11)) k4 k10))
     k1
     k6)))


(define almost-length
  (lambda (f)
    (lambda (ls)
      (if (null? ls)
          0
          (add1 (f (cdr ls)))))))


(define almost-length-cps
  (lambda (f k1)
    (k1 (lambda (ls k2)
      (if (null? ls)
          (k2 0)
          (f (cdr ls) (lambda (fcdrls)
                    (k2 (add1 fcdrls)))))))))

(define almost-length-cps-cps
  (lambda (f k1 k3)
    (k1 (lambda (ls k2 k4)
      (if (null? ls)
          (k2 0 k4)
          (f (cdr ls) (lambda (fcdrls k5)
                    (k2 (add1 fcdrls) k5)) k4))) k3)))


(define id-cps
  (lambda (x k) (k x)))


((why almost-length) '(a b c d e))  ;E

((why-cps almost-length-cps (empty-k)) '(a b c d e) (empty-k))  ;CPS(E)

((why-cps-cps almost-length-cps-cps id-cps (empty-k)) '(a b c d e) id-cps (empty-k)) ;CPS(CPS(E))
