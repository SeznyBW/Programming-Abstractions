#lang racket
; Sezny Watkins
; Jack Egan

; Export all of our top-level definitions so that tests.rkt
; can import them. See tests.rkt.
(provide (all-defined-out))

(define (subset-sum n nums)
  (backtrackss n nums empty (first nums)))

(define (backtrackss n nums sofar curr)
  (cond [(or (equal? (apply + sofar) n) (and (equal? n 0)(empty? sofar))) (reverse sofar)]
        [(empty? nums) #f]
        [else
         (let ([res (backtrackss n (rest nums)
                                 (cons curr sofar)
                                 (first nums))])
           (if res
               res
               (backtrackss n (rest nums) sofar (first nums))))]))


(define (no-repeat n)
  (backtracknr n '(2 3) 1 empty 1))

(define (backtracknr n nums start sofar curr)
  (cond [(equal? (length sofar) n) sofar]
        [(equal? (first nums) start) #f]
        [(feasible sofar (list curr))
         (let ([res (backtracknr n (cons (last nums) (list curr)) (first nums)
                                 (cons curr sofar)
                                 (first nums))])
           (if res
               res
               (backtracknr n (cons (last nums) (list curr)) start sofar (first nums))))]
        [else (backtracknr n (cons (last nums) (list curr)) start sofar (first nums))]))

(define (feasible sofar curr)
  (cond [(empty? sofar) #t]
        [(not (list-prefix? curr sofar)) (feasible (rest sofar) (append curr (list (first sofar))))]
        [else #f]))