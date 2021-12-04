#lang racket

(define (make-add a b)
  (match `(,a ,b)
    [`(,(? number? a) ,(? number? b)) (+ a b)]
    [`(,a 0) a]
    [`(0 ,b) b]
    [`(,a ,b) `(+ ,a ,b)]))

(define (make-sub a b)
  (match `(,a ,b)
    [`(,(? number? a) ,(? number? b)) (- a b)]
    [`(,a 0) a]
    [`(0 ,b) b]
    [`(,a ,b) `(- ,a ,b)]))

(define (make-mult a b)
  (match `(,a ,b)
    [`(,(? number? a) ,(? number? b)) (* a b)]
    [`(,_ 0) 0]
    [`(0 ,_) 0]
    [`(,a 1) a]
    [`(1 ,b) b]
    [`(,a (expt ,b ,n))
     #:when (equal? a b)
     (make-expt a (make-add 1 n))]
    [`((expt ,a ,n) ,b)
     #:when (equal? a b)
     (make-expt a (make-add n 1))]
    [`((expt ,a ,n1) (expt ,b ,n2))
     #:when (equal? a b)
     (make-expt a (make-add n1 n2))]
    [`(,a ,b) `(* ,a ,b)]))

(define (make-expt a x)
  (match `(,a ,x)
    [`(,(? number? a) ,(? number? b)) (expt a b)]
    [`(,a 0) 1]
    [`(,a 1) a]
    [`(,a ,x) `(expt ,a ,x)]))

(define (make-exp a)
  (if (number? a)
      (exp a)
      `(exp ,a)))

(define (make-log a)
  (if (number? a)
      (log a)
      `(log ,a)))

(define (deriv expr var)
  (match expr
    [(? symbol? syl) (if (eq? syl var) 1 0)]
    [(? number? _) 0]
    [`(+ ,a ,b) (make-add (deriv a var) (deriv b var))]
    [`(- ,a ,b) (make-sub (deriv a var) (deriv b var))]
    [`(* ,a ,b) (make-add (make-mult (deriv a var) b)
                          (make-mult a (deriv b var)))]
    [`(/ ,a ,b) (make-mult (make-sub (make-mult (deriv a var)
                                               b)
                                    (make-mult a
                                               (deriv b var)))
                          (make-expt b -2))]
    [`(expt ,a ,(? number? n))
     (make-mult (make-mult n (make-expt a (- n 1)))
                (deriv a var))]
    [`(expt ,a ,b) (deriv (make-exp (make-mult (make-log a)
                                               b)) var)]
    [`(exp ,a) (make-mult (make-exp a) (deriv a var))]
    [`(log ,a) (make-mult (make-expt a -1) (deriv a var))]
    [_ (error "The expr isn't supported\n" expr)]))
    

(define ns (make-base-namespace))

(define (eval-with-vars expr vars)
  (eval `(let ,vars ,expr) ns))

(define (taylor expr var pnt x)
  (define (generator expr n k)
    (let ([val (* k (eval-with-vars expr `([,var ,pnt])))])
      (stream-cons val (stream-map (λ (x) (+ x val))
                                   (generator (deriv expr var)
                                              (+ n 1)
                                              (* k
                                                 (/ 1.0 (+ n 1))
                                                 (- x pnt)))))))
  (generator expr 0 1))
