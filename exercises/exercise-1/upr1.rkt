#lang racket
(define (fast-exp x n)
  (define (sqr x) (* x x))
  (define half (quotient n 2))
  (cond [(= n 0) 1]
        [(= n 1) x]
        [(even? n) (fast-exp (sqr x) half)]
        [else (* x (fast-exp (sqr x) half))]))

(define (roots a b c)
  (define D (- (* b b) (* 4 a c)))
  (cond [(and (= a 0) (= b 0) (= c 0)) +inf.0]
        [(and (= a 0) (= b 0)) 0]
        [(or (= a 0) (= D 0)) 1]
        [(< D 0) 0]
        [else 2]))

(define (fact n)
  (define (loop i res)
    (if (> i n)
        res
        (loop (+ i 1) (* res i))))
  (loop 1 1))

(define (fib n)
  ; Инварианта: curr е i-тото число на Фибоначи
  ; prev е (i-1)-вото число
  (define (loop prev curr i)
    (if (= i n)
        curr
        (loop curr (+ prev curr) (+ i 1))))
  (if (= n 0) 0
      (loop 0 1 1)))

(define (reverse-int n)
  (define (loop newN res)
    (if (= newN 0)
        res
        (loop (quotient newN 10)
              (+ (* 10 res) (remainder newN 10)))))
  (loop n 0))

  (define (prime? n)
  (define (loop i)
    (cond [(= n i) #t]
          [(= (modulo n i) 0) #f]
          [else (loop (+ i 1))]))
  (if (= n 1) #f (loop 2)))

(define (prime-? n)
  (define (loop i)
    (cond [(= n i) #t]
          [(= (modulo n i) 0) #f]
          [else (loop (+ i 2))]))
    (cond [(= n 1) #f]
          [(even? n) (= n 2)]
          [else (loop 3)]))

(define (getLast n) (modulo n 10))
(define (cutLast n) (quotient n 10))

(define (increasing? n) 
  (define last_digit (getLast n))
  (define last_but_one_digit(getLast (cutLast n)))
  (if (= n 0) #t
      (if (> last_digit last_but_one_digit) (increasing? (cutLast n)) #f)))

(define (to-binary n)
  (if (< n 2)
      n
      (+ (* 10 (to-binary (quotient n 2)))
         (remainder n 2))))

(define (to-binary* n)
  (define (loop n res bits)
    (if (= n 0)
        res
        (loop (quotient n 2)
              (+ res (* (remainder n 2) (expt 10 bits)))
              (+ bits 1))))
(loop n 0 0))