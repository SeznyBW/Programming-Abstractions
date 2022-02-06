#lang racket
; Sezny Watkins

(require rackunit rackunit/text-ui rackunit/gui)
(require "hw1.rkt")

; Define tests for atom?.
(define atom?-tests
  (test-suite
   "atom?"
   (test-true "3 is an atom"
              (atom? 3))
   (test-false "Lists are not atoms"
               (atom? '(1 2)))
   (test-false "Pairs are not atoms"
               (atom? (cons 1 2)))
   (test-false "Null is not an atom"
               (atom? null))
   (test-true "false is an atom"
              (atom? #f))))

;define tests for list-of-atom?
(define list-of-atom?-tests
  (test-suite
   "list-of-atoms?"
   (test-true "Empty list"
              (list-of-atom? empty))
   (test-false "list of lists"
               (list-of-atom? (list(list 1 2) (list 2 3))))
   (test-true "list of atoms"
              (list-of-atom? (list #t #f 1 2 "abc" '+)))
   (test-true "one item"
              (list-of-atom? (list 1)))))

;define tests for not-list-of-atom?
(define not-list-of-atom?-tests
  (test-suite
   "not-list-of-atoms?"
   (test-false "Empty list"
              (not-list-of-atom? empty))
   (test-true "list of lists"
               (not-list-of-atom? (list(list 1 2) (list 2 3))))
   (test-false "list of atoms"
              (not-list-of-atom? (list #t #f 1 2 "abc" '+)))
   (test-false "one item"
              (not-list-of-atom? (list 1)))))

;define tests for list-of-int?
(define list-of-int?-tests
  (test-suite
   "list-of-int?"
   (test-true "Empty list"
              (list-of-int? empty))
   (test-true "List of ints large"
              (list-of-int? (list 1 2 3 4 5)))
   (test-true "List of 1 int"
              (list-of-int? (list 1)))
   (test-false "all not int"
               (list-of-int? (list (list 0 2) '+ #f)))
   (test-false "some not int"
               (list-of-int? (list 1 2 #t)))))

;define tests for list-of-same?
(define list-of-same?-tests
  (test-suite
   "list-of-same?"
   (test-true "ints"
              (list-of-same? integer? '(0 1 2 3)))
   (test-false "not-ints"
               (list-of-same? integer? '(0 1 2 #t)))
   (test-true "lists"
              (list-of-same? list? '('(0 1 2) '(1 2 3) '(2 3 4))))
   (test-false "not-lists"
               (list-of-same? list? '('(0 1 2) #f 2)))))


;define tests for make-list-of-same
(define make-list-of-same-tests
  (test-suite
   "make-list-of-same"
   (test-false "not ints"
               ((make-list-of-same integer?) '(25 'foo 18)))
   (test-true "ints"
              ((make-list-of-same integer?) '(0 12 24)))
   (test-true "lists"
              ((make-list-of-same list?) '('(0 1) '(3 1 4) '(0 5 0 4))))
   (test-false "not lists"
               ((make-list-of-same list?) '('( 0 1) "abc" 2)))))

;define tests for all-members
(define all-members-tests
  (test-suite
   "all-members"
   (test-true "all in"
              (all-members '(a c x) '(a b x c d)))
   (test-false "not in"
               (all-members '(a c x) '(a b c)))
   (test-false "empty second"
               (all-members '(a) empty))
   (test-true "empty first"
              (all-members empty '(a b c)))))

;define tests for remove-second
(define remove-second-tests
  (test-suite
   "remove-second"
   (test-equal? "one-instance"
                (remove-second 'x '(a b x c))
                '(a b x c))
   (test-equal? "two-instances"
                (remove-second 'x '(a b x c x d))
                '(a b x c d))
   (test-equal? "three-instances"
                (remove-second 'x '(a b x c x d x e))
                '(a b x c d x e))
   (test-equal? "empty list"
                (remove-second 'x empty)
                empty)
   (test-equal? "singleton"
                (remove-second 'x '(x))
                '(x))))

;define tests for remove-pair
(define remove-pair-tests
  (test-suite
   "remove-pair"
   (test-equal? "empty"
                (remove-pair 'x empty)
                empty)
   (test-equal? "no pairs"
                (remove-pair 'a '(a b c c a b c c a))
                '(a b c c a b c c a))
   (test-equal? "triple"
               (remove-pair 'b '(a b b b a))
               '(a b a))
   (test-equal? "two pair"
               (remove-pair 'a '(a a b b c c a b c a a))
               '(b b c c a b c))
   (test-equal? "two in a row"
               (remove-pair 'b '(a b b b b a))
               '(a a))))

;tests for duplicate
(define duplicate-tests
  (test-suite
   "duplicate"
   (test-equal? "3 single"
                (duplicate 3 'x)
                '(x x x))
   (test-equal? "3 list"
                (duplicate 3 '(a b c))
                '((a b c) (a b c) (a b c)))
   (test-equal? "empty"
               (duplicate 0 'y)
               empty)
   (test-equal? "one"
               (duplicate 1 'z)
               '(z))))

;tests for maximum
(define maximum-tests
  (test-suite
   "maximum"
   (test-equal? "first"
                (maximum '(9 8 7 6))
                9)
   (test-equal? "middle"
                (maximum '( 4 5 6 5 4))
                6)
   (test-equal? "last"
                (maximum '(1 2 3 4 5))
                5)))

;define tests for index-of 
(define index-of-tests
  (test-suite
   "index-of"
   (test-false "not in"
               (index-of '(x y z z y) 'a))
   (test-false "empty"
               (index-of empty 'x))
   (test-equal? "0th"
                (index-of '(x y z z y) 'x)
                0)
   (test-equal? "middle"
                (index-of '(x y z z y) 'y)
                1)
   (test-equal? "last"
                (index-of '(x y z z y a) 'a)
                5)))

        
; Define an All Tests test suite.
(define all-tests
  (test-suite
   "All tests"
   atom?-tests
   list-of-atom?-tests
   not-list-of-atom?-tests
   list-of-int?-tests
   list-of-same?-tests
   make-list-of-same-tests
   all-members-tests
   remove-second-tests
   remove-pair-tests
   duplicate-tests
   maximum-tests
   index-of-tests))

(run-tests all-tests)