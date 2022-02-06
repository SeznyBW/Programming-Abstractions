#lang racket
;Sezny Watkins
;Jack Egan

(provide (all-defined-out))

(define (parse input)
  (define (parse-error)
    (error 'parse "Invalid syntax ~s" input))
  (cond [(number? input) (lit-exp input)]
        [(symbol? input) (var-exp input)]
        [(list? input)
         (cond [(empty? input) (parse-error)]
               [(equal? (first input) 'if) (cond-exp (map parse (rest input)))]
               [(equal? (first input) 'let) (let-exp (map first (second input)) (map parse (map second (second input))) (parse (third input)))]
               [(equal? (first input) 'lambda) (lambda-exp (second input) (parse (third input)))]
               [(equal? (first input) 'set!) (set-exp (second input) (parse (third input)))]
               [(equal? (first input) 'begin) (begin-exp (map parse (rest input)))]
               [(equal? (first input) 'letrec) (letrec-parse input)] 
               [else (app-exp (parse (first input)) (map parse (rest input)))])]
        [else (parse-error)]))

(define (letrec-parse input)
  (let ([syms (map first (second input))]
        [exps (map parse (map second (second input)))]
        [body (third input)]
        [new-syms (map (lambda (s) (gensym)) (map first (second input)))])
    (let-exp syms (map (lambda (s) (lit-exp 0)) syms)
             (let-exp new-syms exps
                      (begin-exp
                        (append (map (lambda (s new-s) (set-exp s new-s)) syms (map parse new-syms))
                                (list (parse body)))))))) 
           

(define (begin-exp exps)
  (list 'begin-exp exps))

(define (begin-exp? exp)
  (if (equal? (first exp) 'begin-exp) #t #f))

(define (begin-exp-exps exp)
  (second exp))

(define (set-exp sym exp)
  (list 'set-exp sym exp))

(define (set-exp? exp)
  (if (equal? (first exp) 'set-exp) #t #f))

(define (set-exp-sym exp)
  (second exp))

(define (set-exp-exp exp)
  (third exp))

(define (lambda-exp parameters body)
  (list 'lambda-exp parameters body))

(define (lambda-exp? exp)
  (if (equal? (first exp) 'lambda-exp) #t #f))

(define (lambda-exp-param l)
  (second l))

(define (lambda-exp-body l)
  (third l))

(define (lit-exp num)
  (list 'lit-exp num))

(define (lit-exp? exp)
  (cond [(equal? (first exp) 'lit-exp) #t]
        [else #f]))

(define (lit-exp-num exp)
  (second exp))

(define (var-exp var)
  (list 'var-exp var))

(define (var-exp? exp)
  (cond [(equal? (first exp) 'var-exp) #t]
        [else #f]))

(define (var-exp-symbol exp)
  (second exp))

(define (prim-proc proc)
  (list 'prim-proc proc))

(define (prim-proc? exp)
  (cond [(equal? (first exp) 'prim-proc) #t]
        [else #f]))

(define (prim-proc-op value)
  (second value))

(define (app-exp proc args)
  (list 'app-exp proc args))

(define (app-exp? exp)
  (cond [(equal? (first exp) 'app-exp) #t]
        [else #f]))

(define (app-exp-proc exp)
  (second exp))

(define (app-exp-args exp)
  (last exp))

(define (cond-exp exp)
  (list 'cond-exp exp))

(define (cond-exp? exp)
  (cond [(equal? (first exp) 'cond-exp) #t]
        [else #f]))

(define (cond-exp-cond exp)
  (first (second exp)))

(define (cond-exp-true exp)
  (second (second exp)))

(define (cond-exp-false exp)
  (third (second exp)))

(define (let-exp symbols bindings body)
  (list 'let-exp symbols bindings body))

(define (let-exp? exp)
  (if (equal? (first exp) 'let-exp) #t #f))

(define (let-exp-symbols exp)
  (second exp))

(define (let-exp-bindings exp)
  (third exp))

(define (let-exp-body exp)
  (fourth exp))