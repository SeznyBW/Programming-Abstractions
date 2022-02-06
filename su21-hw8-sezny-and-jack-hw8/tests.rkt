#lang racket
; Your name(s) here

(require rackunit rackunit/text-ui rackunit/gui)
(require "hw8.rkt")

(define test-stream
  (stream-cons 'x (stream-cons 'y (stream-cons 'z test-stream))))

(define other-tests
  (test-suite
   "other tests"
   (test-equal? "stream-remove-all 1"
                (stream->list (stream-remove-all 1 '(stream 1 2 3 3 2 1)))
                '(stream 2 3 3 2))
   (test-equal? "stream-remove-all 3"
                (stream->list (stream-remove-all 3 '(stream 1 2 3 3 2 1)))
                '(stream 1 2 2 1))
   (test-equal? "stream-replace 1"
                (stream-replace 1 'x '(stream 1 2 3 3 2 1))
                '(stream x 2 3 3 2 x))
   (test-equal? "stream-replace 3"
                (stream-replace 3 'x '(stream 1 2 3 3 2 1))
                '(stream 1 2 x x 2 1))
   (test-equal? "next pair left edge"
                (next-pair (cons 6 1))
                '(1 . 7))
   (test-equal? "next pair other"
                (next-pair (cons 4 3))
                '(5 . 2))
   (test-equal? "stream-merge"
                (stream->list (stream-merge '(stream 1 2 3 4 5 6) '(stream 2 4 6 8 10)))
                '(stream 1 2 3 4 5 6 8 10))
   ))

(define ham-tests
  (test-suite
   "Hamming sequence"
   ; You may wish to uncomment this to test once you have implemented ham.
   ; (test-equal? "1000th element of ham"
   ;              (stream-ref ham 1000)
   ;              51840000)
   ))



; Define an All Tests test suite.
(define all-tests
  (test-suite
   "All tests"
   other-tests
   ham-tests))

(run-tests all-tests)
