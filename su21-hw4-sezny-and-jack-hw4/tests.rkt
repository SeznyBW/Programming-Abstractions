#lang racket
; Sezny Watkins
; Jack Egan

(require rackunit rackunit/text-ui rackunit/gui)
(require "hw4.rkt")
(require "TreeDatatype.rkt")


(define index-tests
  (test-suite
   "index"
   (test-equal? "2 instances"
                (index 3 '(2 4 3 1 2 3))
                2)
   (test-equal? "0 instances"
                (index 3 '(2 4 6 4 2))
                -1)
   (test-equal? "0th index"
                (index 0 '(0 1 2 3))
                0)
   (test-equal? "last index"
                (index 3 '(0 1 2 3))
                3)))

(define replace-tests
  (test-suite
   "replace"
  (test-equal? "2 instances"
               (replace 3 5 '(1 2 3 5 4 3 2 1))
               '(1 2 5 5 4 5 2 1))
  (test-equal? "0 instances"
               (replace 3 5 '(2 4 6 8))
               '(2 4 6 8))
  (test-equal? "0th index"
               (replace 1 2 '(1 3 3 3))
               '(2 3 3 3))
  (test-equal? "last index"
               (replace 1 2 '(3 3 3 1))
               '(3 3 3 2))))

(define weigh-tests
  (test-suite
   "weigh"
   (test-equal? "example"
                (weigh '((duffle 8) (garment-bag 2) (briefcase 5) (valise 7) (steamer-trunk 65)))
                87)
   (test-equal? "empty"
                (weigh empty)
                0)
   (test-equal? "0"
                (weigh '((duffle 0) (garment-bag 0)))
                0)))

(define heaviest-tests
  (test-suite
   "heaviest"
   (test-equal? "example"
                (heaviest '((duffle 8) (garment-bag 2) (briefcase 5) (valise 7) (steamer-trunk 65)))
                'steamer-trunk)
   (test-equal? "empty"
                (heaviest empty)
                'none)
   (test-equal? "first"
                (heaviest '((duffle 100) (garment-bag 2) (briefcase 5) (valise 7) ))
                'duffle)
   (test-equal? "in the middle"
                (heaviest '((duffle 8) (garment-bag 2) (briefcase 100) (valise 7)))
                'briefcase)))

(define child-sum-tests
  (test-suite
   "child-sum"
   (test-equal? "example 1"
                (child-sum T8)
                173)
   (test-equal? "example 2"
                (child-sum T6)
                82)
   (test-equal? "example 3"
                (child-sum empty-tree)
                0)))

(define all-sum-tests
  (test-suite
   "all-sum"
   (test-equal? "T8"
                (all-sum T8)
                293)
   (test-equal? "T6"
                (all-sum T6)
                155)
   (test-equal? "empty"
                (all-sum empty-tree)
                0)))

(define T1n (make-tree 51))
(define T2n (make-tree 23))
(define T3n (make-tree 11))
(define T4n (make-tree 6))
(define T5n (make-tree 18))
(define T6n (make-tree 74 T1n T2n T3n))
(define T7n (make-tree 101 T4n T5n))
(define T8n (make-tree 17 T6n T7n))

(define visit-tree-tests
  (test-suite
   "visit-tree"
   (test-equal? "leaf"
                (visit-tree add1 T1)
                T1n)
   (test-equal? "branch"
                (visit-tree add1 T6)
                T6n)
   (test-equal? "root"
                (visit-tree add1 T8)
                T8n)))

(define sizeof-tests
  (test-suite
   "sizeof"
   (test-equal? "T8"
                (sizeof T8)
                8)
   (test-equal? "T6"
                (sizeof T6)
                4)
   (test-equal? "empty"
                (sizeof null)
                0)
   (test-equal? "leaf"
                (sizeof T1)
                1)))

(define height-tests
  (test-suite
   "height"
   (test-equal? "empty"
                (height empty-tree)
                -1)
   (test-equal? "leaf"
                (height T1)
                0)
   (test-equal? "root"
                (height T8)
                2)))

(define postorder-tests
  (test-suite
   "postorder"
   (test-equal? "leaf"
                (postorder T1)
                '(50))
   (test-equal? "trunk"
                (postorder T8)
                '(50 22 10 73 5 17 100 16))
   (test-equal? "random test"
                (postorder T6)
                '(50 22 10 73))))

(define preorder-tests
  (test-suite
   "preorder"
   (test-equal? "leaf"
                (preorder T1) '(50))
   (test-equal? "trunk"
                (preorder T8) '(16 73 50 22 10 100 5 17))))



; Define an All Tests test suite.
(define all-tests
  (test-suite
   "All tests"
   index-tests
   replace-tests
   weigh-tests
   heaviest-tests
   child-sum-tests
   all-sum-tests
   visit-tree-tests
   sizeof-tests
   height-tests
   postorder-tests
   preorder-tests))

(run-tests all-tests)
