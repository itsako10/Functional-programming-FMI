#lang racket
(define head car)
(define tail cdr)

(define (length* lst)
  (if (null? lst)
      0
      (+ 1 (length* (tail lst)))))

(define (length** lst)
  (define (loop lst res)
    (if (null? lst)
        res
        (loop (tail lst) (+ res 1))))
  (loop lst 0))

(define (reverse lst)
  (if (null? lst)
      '()
      (append (reverse (tail lst)) (list (head lst)))))

(define (reverse** lst)
  (define (loop lst res)
    (if (null? lst)
        res
        (loop (tail lst) (cons (head lst) res))))
  (loop lst '()))

(define (nth n lst)
  (define (loop i n lst1)
    (cond [(> n (length lst)) #f]
         [(= i n) (head lst1)]
         [else (loop (+ 1 i) n (tail lst1))]))
  (loop 0 n lst))

;tazi versiq e po-qka
(define (nth* n lst)
  (cond [(null? lst) #f]
        [(= n 0) (head lst)]
        [else (nth* (- n 1) (tail lst))]))

;po-burzo e ot dolnoto, no puk dolnoto e po-4etimo i kratko za pisane (dolnoto e range*)
(define (range from to)
  (define (loop from to res)
    (if (> from to)
        res
        (loop (+ 1 from) to (cons from res))))
  (reverse** (loop from to '())))

(define (range* from to)
  (if (> from to)
      '()
      (cons from (range* (+ 1 from) to))))

(define (digit-list n)
  (define (loop i n res)
    (if (= 0 n)
        res
        (loop (+ 1 i) (quotient n 10) (cons (remainder n 10) res))))
  (if (= 0 n)
      (list 0)
      (loop 0 n '())))

;tazi se opitvam sam da go napravq, drugite 2 sa ot Andi
(define (take n lst)
  (define (loop i lst1 res)
    (if (> i n)
        res
        (loop (+ 1 i) (tail lst1) (cons (head lst1) res))))
  (reverse** (loop 1 lst '())))

(define (take* n lst)
  (cond [(null? lst) '()]
        [(= 0 n) '()]
        [else (cons (head lst) (take* (- n 1) (tail lst)))]))

;map q imame vgradena i mojem da q izpolzvame
(define (take** n lst)
  (map (lambda (i) (nth i lst)) (range 0 (- n 1))))

(define (drop n lst)
  (cond [(null? lst) '()]
        [(= n 0) lst]
        [else (drop (- n 1) (tail lst))]))

(define (all? p? lst)
  (cond [(null? lst) #t]
        [(p? (head lst)) (all? p? (tail lst))]
        [else #f]))


(define (any? p? lst)
  (cond [(null? lst) #f]
        [(p? (head lst)) #t]
        [else (any? p? (tail lst))]))

(define (sorted? lst)
  (cond [(null? lst) #t]
        [(null? (tail lst)) #t]
        [(< (head lst) (head (tail (lst)))) (sorted? (tail lst))]
        [else #f]))

;integer? e vgradena funkciq, filter su6to e vgradena i moje da q polzvame
(define (uniques lst)
  (if (null? lst)
      '()
      (cons (head lst) (uniques (filter (lambda (x) (not (equal? x (head lst)))) (tail lst))))))

(define (insert val lst)
  (cond [(null? lst) (list val)]
        [(< val (head lst)) (cons val lst)]
        [else (cons (head lst) (insert val (tail lst)))]))

(define (insertion-sort lst)
  (foldr insert '() lst))