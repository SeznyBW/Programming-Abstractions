#lang racket
;Sezny Watkins

; Export all of our top-level definitions so that tests.rkt
; can import them. See tests.rkt
(provide (all-defined-out))

(define (merge lst1 lst2)
  (cond [(empty? lst1) lst2]
        [(empty? lst2) lst1]
        [(> (first lst1) (first lst2)) (cons (first lst2) (merge lst1 (rest lst2)))]
        [else (cons (first lst1) (merge (rest lst1) lst2))]))

;define insertion sort
(define (sort-i lst item)
  (cond [(empty? lst) (list item)]
        [(< item (first lst)) (cons item lst)]
        [else (cons (first lst) (sort-i (rest lst) item))]))

;define sort assist function
;takes in a sorted list and a list of items to add
;then adds each item from the second list to the sorted list
;via insertion sort function
(define (sort-a lsts lsta)
  (cond [(empty? lsta) lsts]
        [else (sort-a (sort-i lsts (first lsta)) (rest lsta))]))


;define sort for list of nums
(define (sort lst)
  (cond [(empty? lst) lst]
        [else (sort-a (list (first lst)) (rest lst))]))

;define helper function fo contains-sublist?
;determines if sublist is at the start of biglist
(define (contains-sublist?-a sublist biglist)
  (cond [(empty? sublist) #t]
        [(empty? biglist) #f]
        [(equal? (first sublist) (first biglist)) (contains-sublist?-a (rest sublist) (rest biglist))]
        [else #f]))


;define contains-sublist?
(define (contains-sublist? sublist biglist)
  (cond [(empty? sublist) #t]
        [(empty? biglist) #f]
        [(and(equal? (first sublist) (first biglist)) (contains-sublist?-a sublist biglist)) #t]
        [else (contains-sublist? sublist (rest biglist))]))

;define helper for remove-sublist
;very similar to helper for contains-sublist?
;except it returns the list rather than true or false
(define (remove-sublist-a sublist biglist)
  (cond [(empty? sublist) biglist]
        [(empty? biglist) biglist]
        [(equal? (first sublist) (first biglist)) (remove-sublist-a (rest sublist) (rest biglist))]
        [else biglist]))

;define remove-sublist
(define (remove-sublist sublist biglist)
  (cond [(empty? sublist) biglist]
        [(empty? biglist) biglist]
        [(equal? (contains-sublist? sublist biglist) #f) biglist]
        [(equal? biglist (remove-sublist-a sublist biglist)) (cons (first biglist) (remove-sublist sublist (rest biglist)))]
        [else (remove-sublist-a sublist biglist)]))

;define phone-number
(define (phone-number person phone-book)
  (cond [(empty? phone-book) 'disconnected]
        [(equal? person (first (first phone-book))) (second (first phone-book))]
        [else (phone-number person (rest phone-book))]))

;define person
(define (person phone-number phone-book)
  (cond [(empty? phone-book) 'disconnected]
        [(equal? phone-number (second (first phone-book))) (first (first phone-book))]
        [else (person phone-number (rest phone-book))]))

;define deepen
(define (deepen lst)
  (cond [(empty? lst) lst]
        [else (cons (list (first lst)) (deepen (rest lst)))]))

;define helper for eval-bin
;takes in an int we can manipulate alongside the list
(define (eval-bin-a lst num)
  (cond [(empty? lst) num]
        [else (eval-bin-a (rest lst) (+ (* 2 num) (first lst)))]))

;define eval-bin
;calculates a binary string as a decimal int
(define (eval-bin lst)
  (eval-bin-a lst 0))

;define substitution
;replaces all instances of old with new
(define (sub old new lst)
  (cond [(empty? lst) lst]
        [(equal? old (first lst)) (cons new (sub old new (rest lst)))]
        [else (cons (first lst) (sub old new (rest lst)))]))

;define subs
(define (subs old-lst new-lst lst)
  (cond [(empty? old-lst) lst]
        [else (subs (rest old-lst) (rest new-lst) (sub (first old-lst) (first new-lst) lst))]))
