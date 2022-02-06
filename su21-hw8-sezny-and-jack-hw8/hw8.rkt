#lang racket
;Sezny Watkins
; Jack Egan

(require racket/stream)
(require "keyboard.rkt")

(provide (all-defined-out))


(define (stream-remove-all x s)
  (cond [(stream-empty? s) s]
        [(equal? x (stream-first s)) (stream-remove-all x (stream-rest s))]
        [else (stream-cons (stream-first s) (stream-remove-all x (stream-rest s)))]))

(define (stream-replace x y s)
  (cond [(stream-empty? s) s]
        [(equal? x (stream-first s)) (cons y (stream-replace x y (stream-rest s)))]
        [else (cons (stream-first s) (stream-replace x y (stream-rest s)))]))

(define (next-pair p)
  (if (equal? (cdr p) 1)
      (cons 1 (add1 (car p)))
      (cons (add1 (car p)) (- (cdr p) 1))))



(define (stream-merge s1 s2)
  (cond [(stream-empty? s1) s2]
        [(stream-empty? s2) s1]
        [(equal? (stream-first s1) (stream-first s2))
         (stream-cons (stream-first s1) (stream-merge (stream-rest s1) (stream-rest s2)))]
        [(> (stream-first s1) (stream-first s2)) (stream-cons (stream-first s2) (stream-merge s1 (stream-rest s2)))]
        [else (stream-cons (stream-first s1) (stream-merge (stream-rest s1) s2))]))  



(define ham
  (stream-cons 1 (stream-merge
                  (stream-merge (stream-map (λ (n) (* 2 n)) ham) (stream-map (λ (n) (* 3 n)) ham))
                  (stream-map (λ (n) (* 5 n)) ham))))

(define (integers-from n)
  (stream-cons n (integers-from (add1 n))))

(define fact-stream
  (stream-cons 1 (stream-mult fact-stream (integers-from 1))))

(define (stream-mult s t)
  (cond [(stream-empty? s) empty-stream]
        [(stream-empty? t) empty-stream]
        [else
         (stream-cons (* (stream-first s)
                         (stream-first t))
                      (stream-mult (stream-rest s)
                                  (stream-rest t)))]))

(define (stream-add s t)
  (cond [(stream-empty? s) empty-stream]
        [(stream-empty? t) empty-stream]
        [else
         (stream-cons (+ (stream-first s)
                         (stream-first t))
                      (stream-add (stream-rest s)
                                  (stream-rest t)))]))


(define (partial-sums s)
  (cond [(stream-empty? s) empty-stream]
        [else
         (letrec ([sums (stream-add s (stream-cons 0 sums))])
           sums)]))

(define (powers x)
  (stream-cons 1 (stream-map (lambda (n) (* x n)) (powers x))))

(define sin-help
  (stream-cons 0 (stream-cons 1 (stream-cons 0 (stream-cons -1 sin-help)))))

(define e-coeffs
  (stream-map (λ (n) (/ 1.0 n)) fact-stream))

(define sin-coeffs
  (stream-mult sin-help e-coeffs))

(define cos-help
  (stream-cons 1 (stream-cons 0 (stream-cons -1 (stream-cons 0 cos-help)))))

(define cos-coeffs
  (stream-mult cos-help e-coeffs))

(define (sin-approx x)
  (partial-sums (stream-mult (powers x) sin-coeffs)))

(define (cos-approx x)
  (partial-sums (stream-mult (powers x) cos-coeffs)))

(define (gab-help keys a)
  (cond [(stream-empty? keys) keys]
        [(equal? (stream-first keys) 'a)
         (cond [(equal? a 0) (gab-help (stream-rest keys) 1)]
               [else (stream-cons 'b (gab-help (stream-rest keys) 0))])]
        [(equal? a 0) (stream-cons (stream-first keys) (gab-help (stream-rest keys) 0))]
        [else (stream-cons 'a
                           (stream-cons
                            (stream-first keys)
                            (gab-help (stream-rest keys) 0)))]))

(define grune-a-b
  (lambda (keys)
    (gab-help keys 0)))

(define (filter-help keys a b n)
  (cond [(stream-empty? keys) keys]
        [(equal? (stream-first keys) a)
         (cond [(equal? n 0) (filter-help (stream-rest keys) 1)]
               [else (stream-cons b (filter-help (stream-rest keys) 0))])]
        [(equal? n 0) (stream-cons (stream-first keys) (filter-help (stream-rest keys) 0))]
        [else (stream-cons a
                           (stream-cons
                            (stream-first keys)
                            (filter-help (stream-rest keys) 0)))]))

(define (grune a b)
  (lambda (keys)
    (filter-help keys a b 0)))