#lang racket
; Your name(s) here

(require rackunit rackunit/text-ui rackunit/gui)
(require "hw3.rkt")

(define firsts-tests
  (test-suite
   "firsts"
   (test-equal? "example"
                (firsts '((a b c) (d e f) (g h i)))
                '(a d g))
   (test-equal? "empty"
                (firsts null)
                null)
   (test-equal? "one thing"
                (firsts '((a b c)))
                '(a))
   (test-equal? "one-item lists"
               (firsts '((a) (b) (c)))
               '(a b c))))

(define rests-tests
  (test-suite
   "rests"
   (test-equal? "example"
                (rests '((a b c) (d e f) (g h i)))
                '((b c) (e f) (h i)))
   (test-equal? "empty"
                (rests null)
                null)
   (test-equal? "one thing"
                (rests '((a b c)))
                '((b c)))
   (test-equal? "one-item list"
                (rests '((a) (b) (c)))
                '(() () ()))))

(define vec-+-tests
  (test-suite
   "vec-+"
   (test-equal? "example"
                (vec-+ '(1 2 3) '(4 5 6))
                '(5 7 9))
   (test-equal? "empty"
                (vec-+ empty empty)
                empty)
   (test-equal? "1-d"
                (vec-+ '(1) '(4))
                '(5))))

(define dot-product-tests
  (test-suite
   "dot-product"
   (test-equal? "example"
                (dot-product '(1 2 3) '(4 5 6))
                32)
   (test-equal? "empty"
                (dot-product empty empty)
                0)
   (test-equal? "one-item"
                (dot-product '(1) '(4))
                4)))

(define mat-vec-*-tests
  (test-suite
   "mat-vec-*"
   (test-equal? "example 1"
                (mat-vec-* '((1 4 7) (2 5 8) (3 6 9)) '(1 2 3))
                '(30 36 42))
   (test-equal? "example 2"
                (mat-vec-* '((2 3 4) (1 1 1)) '(1 0 1))
                '(6 2))
   (test-equal? "empty"
                (mat-vec-* empty empty)
                empty)))

(define transpose-tests
  (test-suite
   "transpose"
   (test-equal? "example 1"
                (transpose '((1 4 7) (2 5 8) (3 6 9)))
                '((1 2 3) (4 5 6) (7 8 9)))
   (test-equal? "example 2"
                (transpose '((1 2 3) (4 5 6)))
                '((1 4) (2 5) (3 6)))
   (test-equal? "empty"
                (transpose empty)
                empty)
   (test-equal? "one-d"
                (transpose '((1 2 3)))
                '((1) (2) (3)))))

(define mat-mat-*-tests
  (test-suite
   "mat-mat-*"
   (test-equal? "example"
                (mat-mat-* '((1 0 1) (2 1 1)) '((1 2) (1 0) (1 1)))
                '((2 3) (4 5)))
   (test-equal? "random"
                (mat-mat-* '((1 2)(3 4)) '((5 6) (7 8)))
                '((19 22) (43 50)))
   (test-equal? "empty"
                (mat-mat-* empty empty)
                empty)))

(define sum-tests
  (test-suite
   "sum"
   (test-equal? "example"
                (sum '((1 (2)) (((4))) 5))
                12)
   (test-equal? "empty"
                (sum empty)
                0)
   (test-equal? "0"
                (sum '((0) (((0))) 0))
                0)
   (test-equal? "1 item"
                (sum '(5))
                5)))

(define flatten-tests
  (test-suite
   "flatten"
   (test-equal? "example 1"
                (flatten '(x y z z y))
                '(x y z z y))
   (test-equal? "example 2"
                (flatten '(a (x (y)) (((y)) y z)))
                '(a x y y y z))
   (test-equal? "empty"
                (flatten empty)
                empty)))

(define element-of?-tests
  (test-suite
   "element-of?"
   (test-true "example 1"
              (element-of? 3 '(2 1 (4 2 (5 3) 1))))
   (test-false "example 2"
               (element-of? 'x '(a (b c (d)) e f)))
   (test-false "empty list"
               (element-of? 'x empty))))

(define map-to-tests
  (test-suite
   "map-to"
   (test-equal? "example"
                (map-to add1 '(3 (4 5)))
                '(4 (5 6)))
   (test-equal? "extra parantheses"
                (map-to add1 '((3 (4 5))))
                '((4 (5 6))))
   (test-equal? "empty"
                (map-to add1 empty)
                empty)))

; Define an All Tests test suite.
(define all-tests
  (test-suite
   "All tests"
   firsts-tests
   rests-tests
   vec-+-tests
   dot-product-tests
   mat-vec-*-tests
   transpose-tests
   mat-mat-*-tests
   sum-tests
   flatten-tests
   element-of?-tests
   map-to-tests))

(run-tests all-tests)
