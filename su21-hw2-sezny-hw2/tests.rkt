#lang racket
;Sezny Watkins

(require rackunit rackunit/text-ui rackunit/gui)
(require "hw2.rkt")


; Define tests for merge.
(define merge-tests
  (test-suite
   "merge"
   (test-equal? "Odds and evens"
                (merge '(1 3 5 7 9) '(2 4 6 8 10))
                '(1 2 3 4 5 6 7 8 9 10))
   
   (test-equal? "Two empty lists"
                (merge null null)
                null)
   (test-equal? "example"
                (merge '(1 4 5) '(2 3 4 6))
                       '(1 2 3 4 4 5 6))
   (test-equal? "first empty"
                (merge null '(2 3 4))
                '(2 3 4))
   (test-equal? "second empty"
                (merge '(2 3 4) null)
                '(2 3 4))))

;define tests for sort
(define sort-tests
  (test-suite
   "sort"
   (test-equal? "example"
                (sort '(5 1 8 3 7))
                '(1 3 5 7 8))
   (test-equal? "sorted"
                (sort '(1 3 5 7 8))
                '(1 3 5 7 8))
   (test-equal? "empty"
                (sort null)
                null)
   (test-equal? "one"
                (sort '(1))
                '(1))))

;define tests for contains-sublist?
(define contains-sublist?-tests
  (test-suite
   "contains-sublist?"
   (test-true "example 1"
                (contains-sublist? '(2 3 4) '(1 2 3 4 5)))
   (test-false "example 2"
               (contains-sublist? '(2 3 4) '(1 2 5 3 4)))
   (test-false "empty biglist"
               (contains-sublist? '(1 2 3) null))
   (test-true "empty sublist"
              (contains-sublist? null '(1 2 3)))
   (test-false "small biglist"
               (contains-sublist? '(1 2 3) '(1)))))

;define tests for remove-sublist
(define remove-sublist-tests
  (test-suite
   "remove-sublist"
   (test-equal? "example 1"
                (remove-sublist '(2 3 4) '(1 2 3 4 5))
                '(1 5))
   (test-equal? "example 2"
                (remove-sublist '(2 3 4) '(1 2 5 3 4))
                '(1 2 5 3 4))
   (test-equal? "sub empty"
                (remove-sublist null '(1 2 3 4))
                '(1 2 3 4))
   (test-equal? "big empty"
                (remove-sublist '(1 2 3) null)
                null)
   (test-equal? "small biglist"
                (remove-sublist '(1 2 3) '(1))
                '(1))))

;define phone book for phone-number tests
(define phone-book
  '((barbara 775-1234)
    (luke 774-2839)
    (nick 775-0912)
    (valerie 775-9043)))

;define tests for phone-number
(define phone-number-tests
  (test-suite
   "phone-number"
   (test-equal? "example"
                (phone-number 'nick phone-book)
                '775-0912)
   (test-equal? "missing person"
                (phone-number 'sezny phone-book)
                'disconnected)
   (test-equal? "empty phone-book"
                (phone-number 'nick null)
                'disconnected)))

;define person tests
(define person-tests
  (test-suite
   "person"
   (test-equal? "example"
                (person '775-0912 phone-book)
                'nick)
   (test-equal? "missing number"
                (person '339-2218 phone-book)
                'disconnected)
   (test-equal? "empty phone-book"
                (person '775-0912 null)
                'disconnected)))

;define tests for deepen
(define deepen-tests
  (test-suite
   "deepen"
   (test-equal? "example 1"
                (deepen '(a b c))
                '((a) (b) (c)))
   (test-equal? "example 2"
                (deepen '(a (b (c d)) e))
                '((a) ((b (c d))) (e)))
   (test-equal? "empty list"
                (deepen null)
                null)
   (test-equal? "one item"
                (deepen '(a))
                '((a)))
   (test-equal? "one list"
                (deepen '((a b c)))
                '(((a b c))))))

;define tests for eval-bin
(define eval-bin-tests
  (test-suite
   "eval-bin"
   (test-equal? "example 1"
                (eval-bin '(1 0 1 1))
                11)
   (test-equal? "example 2"
                (eval-bin '(1 1 0))
                6)
   (test-equal? "empty list"
                (eval-bin null)
                0)
   (test-equal? "one 0"
                (eval-bin '(0))
                0)
   (test-equal? "one 1"
               (eval-bin '(1))
               1)))

;define tests for substitution
(define sub-tests
  (test-suite
   "sub"
   (test-equal? "example"
                (sub 'a 'x '(a b r a c a d a b r a))
                '(x b r x c x d x b r x))
   (test-equal? "empty list"
                (sub 'a 'x null)
                null)
   (test-equal? "no subs"
                (sub 'a 'x '(b c d e f g))
                '(b c d e f g))
   (test-equal? "all subs"
                (sub 'a 'x '(a a a a a))
                '(x x x x x))))

;define tests for subs
(define subs-tests
  (test-suite
   "subs"
   (test-equal? "example 1"
                (subs '(b) '(m) '(b o b))
                '(m o m))
   (test-equal? "example 2"
                (subs '(b o) '(m u) '(b o b))
                '(m u m))
   (test-equal? "empty list"
                (subs '(b o) '(m u) null)
                null)
   (test-equal? "no changes"
                (subs null null '(b o b))
                '(b o b))))


; Define an All Tests test suite.
(define all-tests
  (test-suite
   "All tests"
   merge-tests
   sort-tests
   contains-sublist?-tests
   remove-sublist-tests
   phone-number-tests
   person-tests
   deepen-tests
   eval-bin-tests
   sub-tests
   subs-tests))

(run-tests all-tests)
