#lang racket
(define head car)
(define tail cdr)

;(define (begins-with? lst1 lst2)
;  (cond [(null? lst1) #t]
;        [(null? lst2) #f]
;        [(equal? (head lst1) (head lst2)) (begins-with? (tail lst1) (tail lst2))]
;        [else #f]))

(define (sublist? lst1 lst2)
  (cond [(null? lst2) (null? lst1)]
        [(begins-with? lst1 lst2) #t]
        [else (sublist? lst1 (tail lst2))]))

(define (begins-with? lst1 lst2)
  (or (null? lst1)
      (and (not (null? lst2))
           (equal? (head lst1) (head lst2))
           (begins-with? (tail lst1) (tail lst2)))))

(define (set-insert x lst)
  (if (member x lst)
      lst
      (cons x lst)))

(define (make-set lst)
  (foldr set-insert '() lst))

(define (length lst)
  (foldr (lambda (el res) (+ res 1)) 0 lst))

;az q napisah
(define (count* element lst)
  (foldr (lambda (el res) (if (equal? el element) (+ res 1) res)) 0 lst))

;Andi q napisa
(define (count x lst)
  (length (filter (lambda (el) (equal? el x)) lst)))

(define (histogram lst)
  (map (lambda (el) (cons el (count el lst))) (make-set lst)))

;napravih go prosto za uprajnenie
(define (sum a b)
  (lambda () (+ a b)))

(define (quick-sort lst)
  (define (gen pred)
    (lambda (el) (pred el (head lst))))
  (if (or (null? lst)
          (null? (tail lst)))
      lst;
      (append (quick-sort (filter (gen <) lst)) 
                          (filter (gen =) lst)
              (quick-sort (filter (gen >) lst)))))

(define (null-row? m)
  (null? m))

(define (null-cols? m)
  (or (null? m) (null? (head m))))

(define (head-rows m)
  (head m))

(define (head-cols m)
  (map head m))

(define (tail-rows m)
  (tail m))

(define (tail-cols m)
  (map tail m))

(define (cons-rows row m)
  (cons row m))

(define (cons-cols col m)
  (map cons col m))
  ;(cons-cols '(1 2 3) '((4 5) (8 10) (9 6)))
;-> '((1 4 5) (2 8 10) (3 9 6))

(define (transpose m)
  (if (null-cols? m)
      '()
      (cons (head-cols m) (transpose (tail-cols m)))))