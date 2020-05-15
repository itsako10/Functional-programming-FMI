#lang racket
(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

;(define (!! n)
;  (accumulate * 1
;              (if (even? n) 2 1) n
;              (lambda (i) i) (lambda (i) (+ i 2))))

(define (!! n)
  (cond [(= (modulo n 2) 0) (accumulate * 1 2 n (lambda (x) x) (lambda (x) (+ x 2)))]
       [(accumulate * 1 1 n (lambda (x) x) (lambda (x) (+ x 2)))]))

(define (nchk n k)
  (accumulate * 1 1 k (lambda (x) (/ (- n (- x 1)) x)) (lambda (x) (+ x 1))))

(define (2^ n)
  (accumulate * 1 1 n (lambda (x) 2) (lambda (x) (+ x 1))))

(define (2^* n)
  (accumulate + 0 0 n (lambda (x) (nchk n x)) (lambda (x) (+ x 1))))

(define (divisors-sum n)
  (accumulate + 0 1 n (lambda (x) (if (= (remainder n x) 0) x 0)) (lambda (x) (+ x 1))))

(define (filter-accum p? op nv a b term next)
  (cond [(> a b) nv]
        [(p? a) (op (term a) (filter-accum p? op nv (next a) b term next))]
        [else (filter-accum p? op nv (next a) b term next)]))

(define (divisors-sum* n)
  (filter-accum (lambda (x) (= (remainder n x) 0)) + 0 1 n (lambda (x) x) (lambda (x) (+ x 1))))

(define (count p? a b)
  (accumulate + 0 a b (lambda (x) (if (p? x) 1 0)) (lambda (x) (+ x 1))))

(define (all? p? a b)
  (accumulate (lambda (x y) (and x y))
              #t
              a b
              p? 1+))

(define (any? p? a b)
  (not (all? (lambda (x) (not (p? x))) a b)))

(define (prime? n)
  (filter-accum (lambda (x) (= (remainder n x) 0)) (lambda (x y) (and x y)) #t 2 (- n 1) (lambda (x) #f) (lambda (x) (+ x 1))))