# Упражнение №5
За удобство и консистентност ще използваме следните "стандартни" функции за работа с дървета:

(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3))
           (tree? (cadr t))
           (tree? (caddr t))))

(define empty-tree '())

(define (make-tree root left right) (list root left right))      ; не искаме просто (define make-tree list) - защо?

(define (make-leaf root) (make-tree root empty-tree empty-tree)) ; за удобство

(define root-tree car)

(define left-tree cadr)

(define right-tree caddr)

(define empty-tree? null?)

(define t
  (make-tree 10
             (make-tree 7
                        (make-leaf 10)
                        (make-leaf 2))
             (make-tree 3
                        (make-tree 4
                                   (make-leaf 1)
                                   (make-leaf 2))
                        empty-tree)))

Зад.1. Да се напише функция (tree-sum t), която намира сумата на всички елементи на дървото t.

Зад.2. Да се напише функция (tree-level k t), която връща списък от всички стойности във възли на дълбочина k (тоест разстояние k от корена).

Зад.3. Да се напише функция (tree->list t), която връща списък от всички елементи на дървото, получени при обхождане ляво-корен-дясно.

Зад.4. Да се напише функция (bst-insert val t), която вмъква стойността val в двоичното наредено дърво t.

Зад.5. Да се напише функция (sort lst), която сортира списъка lst.
