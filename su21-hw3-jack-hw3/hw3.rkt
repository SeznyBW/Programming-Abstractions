#lang racket
; HW3
; Jack Egan 

; Export all of our top-level definitions so that tests.rkt
; can import them. See tests.rkt.
(provide (all-defined-out))

(define (firsts lsts)
  (map first lsts))
(define (rests lsts)
  (map rest lsts))

(define (vec-+ vec1 vec2)
  (map + vec1 vec2))

(define (dot-product vec1 vec2)
  (apply + (map * vec1 vec2)))
(define (mat-vec-* mat vec)
  (map dot-product mat (helper-mat mat vec '() 0)))

(define (helper-mat mat vec newlst acc)        ;This function's purpose is to duplicate the vector enough to call map in the main function and have it work.
  (cond[(equal? (length mat) acc) newlst]      ; If mat has 3 rows, this function will return a list of the vector 3 times. 
       [else (helper-mat mat vec (cons vec newlst) (+ acc 1))]))
(define (transpose mat)
  (cond [(empty? mat) empty]
        [else (apply map list mat)]))
; (define (mat-mat-* lhs rhs)
 ;  (cons (map dot-product (transpose rhs) lhs) (map dot-product  lhs (reverse (transpose rhs)))))

(define (mat-mat-* lhs rhs)
  (transpose(mat-mat-*-transposed lhs (transpose rhs))))

(define (mat-mat-*-transposed lhs rhs)
  (cond [(empty? rhs) empty]
        [else (cons (map dot-product lhs (helper-mat lhs (first rhs) '() 0))
        (mat-mat-*-transposed lhs (rest rhs)))]))

(define (sum lst)
  (cond [(number? lst) lst]
        [(list? lst) (apply + (map sum lst))]
        [else
         (error 'sum
                "v isn't a number of a list"
                lst)]))
(define (flatten lst)
  (cond
    [(empty? lst) '()]
    [(list? (first lst))
    (flatten (append (first lst)(rest lst)))]
    [else (cons (first lst) (flatten (rest lst)))]))
 (define (element-of? a lst )
   
(define (elementflat? a lst)
  (cond[(empty? lst) #f]
       [(equal? (first lst) a) #t]
       [else (elementflat? a (rest lst))]))
   (elementflat? a (flatten lst)))

(define (map-to f lst)
  (cond [(empty? lst) lst]
        [(list? (first lst))
         (cons (map-to f (first lst)) (map-to f (rest lst)))]
        [else (cons (f (first lst)) (map-to f (rest lst)))]))



