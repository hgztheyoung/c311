#lang racket

(define ack
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack (sub1 m) 1 k)]
      [else (ack m (sub1 n) (lambda (v) (ack (sub1 m) v k)))])))

(define ack-reg-driver
  (lambda (m n)
    (define m* 'dummy)
    (define n* 'dummy)
    (define k* 'dummy)
    (define v* 'dummy)

    (define apply-k
      (lambda ()
        (match k*
          [`(empty-k) v*]
          [`(else-k ,m ,k)
           (begin
             (set! m* (sub1 m))
             (set! n* v*)
             (set! k* k)
             (ack-reg))])))

    (define (empty-k)
      `(empty-k))
    (define (else-k m* k*)
      `(else-k ,m* ,k*))
    
    (define ack-reg
      (lambda ()
        (cond
          [(zero? m*)
           (begin
             (set! v* (add1 n*))
             (set! k* k*)
             (apply-k))]
          [(zero? n*)
           (begin         
             (set! k* k*)
             (set! m* (sub1 m*))
             (set! n* 1)
             (ack-reg))]
          [else
           (begin         
             (set! k* (else-k m* k*))
             (set! m* m*)
             (set! n* (sub1 n*))
             (ack-reg))])))

    (begin
      (set! m* m)
      (set! n* n)
      (set! k* (empty-k))
      (ack-reg)
      v*)))

(define depth
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(pair? (car ls))
       (depth (car ls)
              (lambda (l)
                (depth (cdr ls)
                       (lambda (r)
                         (let ((l (add1 l)))
                           (if (< l r) (k r) (k l)))))))]
      [else (depth (cdr ls) k)])))

(define depth-reg-driver
  (lambda (ls)
    (define ls* 'dummy)
    (define k* 'dummy)
    (define v* 'dummy)

    (define (empty-k)
      `(empty-k))
    (define (carls-k-1 ls* k*)
      `(carls-k-1 ,ls* ,k*))
    (define (carls-k-2 v* k)
      `(carls-k-2 ,v* ,k))
    (define apply-k
      (lambda ()
        (match k*
          [`(empty-k) v*]
          [`(carls-k-1 ,ls ,k)
           (begin
             (set! k* (carls-k-2 v* k))
             (set! ls* (cdr ls))
             (depth-reg))]
          [`(carls-k-2 ,l ,k) 
           (let ((l (add1 l)))
             (if (< l v*)
                 (begin
                   (set! k* k)
                   (set! v* v*)
                   (apply-k))
                 (begin
                   (set! k* k)
                   (set! v* l)
                   (apply-k))))])))
    
    (define depth-reg
      (lambda ()
        (cond
          [(null? ls*)
           (begin
             (set! v* 1)
             (set! k* k*)
             (apply-k))]
          [(pair? (car ls*))
           (begin
             (set! ls* (car ls*))
             (set! k* (carls-k-1 ls* k*))
             (depth-reg))]
          [else
           (begin
             (set! ls* (cdr ls*))
             (set! k* k*)
             (depth-reg))])))
    
    (begin
      (set! ls* ls)
      (set! k* (empty-k))
      (depth-reg)
      v*)))

(define fact
  (lambda (n k)
    ((lambda (fact k)
       (fact fact n k))
     (lambda (fact n k)
       (cond
         [(zero? n) (k 1)]
         [else (fact fact (sub1 n) (lambda (v) (k (* n v))))]))
     k)))

(define fact-reg-driver
  (lambda (n)
    (define (empty-k)
     `(empty-k))
    (define (fact-k n* k*)
      `(fact-k ,n* ,k*))
    (define n* 'dummy)
    (define k* 'dummy)
    (define v* 'dummy)
    (define (apply-k)
      (match k*
        [`(empty-k) v*]
        [`(fact-k ,n ,k)
         (begin
           (set! n* n)
           (set! k* k)
           (set! v* (* n* v*))
           (apply-k))]))
    (define (fact-reg)
      ((lambda (fact)
         (fact fact))
       (lambda (fact)
         (cond
           [(zero? n*)
            (begin
              (set! k* k*)
              (set! v* 1)
              (apply-k))]
           [else
            (begin
              (set! k* (fact-k n* k*))              
              (set! n* (sub1 n*))
              (fact fact))]))))
   (begin
     (set! n* n)
     (set! k* (empty-k))
     (fact-reg)
     v*)))

(define pascal
  (lambda (n k)
    (let ((pascal
           (lambda (pascal k)
             (k (lambda (m a k)
                  (cond
                    [(> m n) (k '())]
                    [else (let ((a (+ a m)))
                            (pascal pascal (lambda (f) (f (add1 m) a (lambda (v) (k (cons a v)))))))]))))))
      (pascal pascal (lambda (f) (f 1 0 k))))))

(define pascal-reg-driver
  (lambda (n)
    (define n* 'dummy)
    (define k* 'dummy)
    (define v* 'dummy)

    (define (empty-k) `(empty-k))
    (define (apply-k)
      (match k*
        [`(empty-k) `(empty-k)]
        [`(let-body-k ,k) (begin
                            (set! k* k)
                            (v* 1 0))]
        [`(else-k-1 ,m ,a ,k) (begin
                                (set! k* `(else-k-2 ,a ,k))
                                (v* (add1 m) a))]
        [`(else-k-2 ,a ,k) (begin (set! v* (cons a v*))
                               (set! k* k)
                               (apply-k))]))
    (define pascal-reg
      (lambda ()
        (let ([pascal (lambda (pascal)
                        (begin
                          (set! v* (lambda (m a)
                                     (cond
                                       [(> m n*) (begin (set! v* '())
                                                        (apply-k))]
                                       [else (let ([a (+ a m)])
                                               (begin
                                                 (set! k* `(else-k-1 ,m ,a ,k*))
                                                (pascal pascal)))])))
                          (set! k* k*)
                          (apply-k)))])
          (begin
            (set! k* `(let-body-k ,k*))
            (pascal pascal)))))
    (begin
      (set! n* n)
      (set! k* (empty-k))
      (pascal-reg)
      v*)))
