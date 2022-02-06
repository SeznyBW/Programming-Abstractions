#lang racket
; Sezny Watkins
; Jack Egan

(require "TreeDatatype.rkt")
; Export all of our top-level definitions so that tests.rkt
; can import them. See tests.rkt.
(provide (all-defined-out))

(define (index x lst)
  (foldr (lambda (head result)
            (cond [(equal? x head) 0]
                  [(equal? result -1) result]
                  [else (+ result 1)]))
           -1
           lst))

(define (replace a b lst)
  (foldr (lambda (head result)
           (if (equal? head a)
               (cons b result)
               (cons head result)))
         empty
         lst))

(define (weigh bags)
  (foldr (lambda (head result)
           (+ (second head) result))
         0
         bags))

(define (heaviest bags)
  (first (foldr (lambda (head result)
           (if (> (second head) (second result))
               head
               result))
         '(none 0)
         bags)))

(define (child-sum t)
  (if (or (empty-tree? t) (leaf? t))
      0
      (apply + (map tree-value (tree-children t)))))

(define (all-sum t)
  (cond [(empty-tree? t) 0]
        [(leaf? t) (tree-value t)]
        [else (+ (tree-value t) (apply + (map all-sum (tree-children t))))]))


(define (remove-parens lst)
    (append* (map (lambda (x)
                  (cond[(list? x) x]
                       [else (list x)]))
                    lst)))

(define (visit-tree f t)
   (letrec ([helper
            (lambda (tree)
              (cond [(empty-tree? tree) null]
                    [(leaf? tree) (make-tree (f (tree-value tree)))]
                    [else (remove-parens (make-tree
                           (f (tree-value tree))
                           (map helper (tree-children tree))))]))])
   (helper t)))
     




(define (sizeof t)
  (cond [(empty-tree? t) 0]
        [else (+ 1 (apply + (map sizeof (tree-children t))))]))


(define (height t)
  (cond [(empty-tree? t) -1]
        [else (+ 1 (foldr (lambda (head result)
                           (cond [(> result (height head)) result]
                                 [else (height head)]))
                          -1
                          (tree-children t)))]))

(define (flatten lst)
  (cond
    [(empty? lst) '()]
    [(list? (first lst))
    (flatten (append (first lst)(rest lst)))]
    [else (cons (first lst) (flatten (rest lst)))]))

(define (preorder-h t)
  (cond [(leaf? t) (list(tree-value t))]
        [else (cons (tree-value t)(map preorder-h (tree-children t)))]))
(define (preorder t)
  (flatten (preorder-h t)))

(define (postorder t)
  (cond[(leaf? t) (list (tree-value t))]
       [else (flatten (list (map postorder (tree-children t)) (tree-value t)))]))
                          