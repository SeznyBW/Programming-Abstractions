#lang racket
; Sezny Watkins

; Export all of our top-level definitions so that tests.rkt
; can import them. See tests.rkt.
(provide (all-defined-out))


(define (atom? x)
  (if (or (pair? x) (null? x)) #f #t))

(define (list-of-atom? lst)
  (cond [(empty? lst) #t]
        [(atom? (first lst))(list-of-atom? (rest lst))]
        [else #f]))


(define (not-list-of-atom? lst)
  (cond [(empty? lst) #f]
        [(atom? (first lst)) #f]
        [(empty? (rest lst)) #t]
        [else (not-list-of-atom? (rest lst))]))



(define (list-of-int? lst)
  (cond [(empty? lst) #t]
        [(integer? (first lst)) (list-of-int? (rest lst))]
        [else #f]))

(define (list-of-same? type lst)
  (cond [(empty? lst) #t]
        [(type (first lst)) (list-of-same? type (rest lst))]
        [else #f]))

(define (make-list-of-same type)
  (lambda (lst)
    (cond [(empty? lst) #t]
          [(type (first lst)) ((make-list-of-same type) (rest lst))]
          [else #f])))

(define (contains itm lst2)
  (cond[(empty? lst2) #f]
       [(equal? itm (first lst2)) #t]
       [else (contains itm (rest lst2))]))

(define (all-members lst1 lst2)
  (cond [(empty? lst1) #t]
        [(contains (first lst1) lst2) (all-members (rest lst1) lst2)]
        [else #f]))

(define (remove-first x lst)
  (cond [(empty? lst) lst]
        [(equal? x (first lst)) (rest lst)]
        [else (cons (first lst) (remove-first x (rest lst)))]))

(define (remove-second x lst)
  (cond [(empty? lst) lst]
        [(equal? x (first lst)) (cons (first lst) (remove-first x (rest lst)))]
        [else (cons (first lst) (remove-second x (rest lst)))]))


(define (remove-pair x lst)
  (cond [(empty? lst) lst]
        [(empty? (rest lst)) lst]
        [(and (equal? x (first lst)) (equal? x (second lst))) (remove-pair x (rest (rest lst)))]
        [else (cons (first lst) (remove-pair x (rest lst)))]))

(define (duplicate n exp)
  (cond [(= n 0) null]
        [else (cons exp (duplicate (- n 1) exp))]))

(define (maximum lst)
  (cond [(empty? (rest lst)) (first lst)]
        [(> (first lst) (maximum (rest lst))) (first lst)]
        [else (maximum (rest lst))]))

(define (index-of lst x)
  (cond [(empty? lst) #f]
        [(equal? x (first lst)) 0]
        [(index-of (rest lst) x) (+ 1 (index-of (rest lst) x))]
        [else #f]))