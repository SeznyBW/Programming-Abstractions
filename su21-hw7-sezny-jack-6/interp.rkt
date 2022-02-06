#lang racket
;Sezny Watkins
;Jack Egan

(require "parse.rkt")
(require "env.rkt")

(provide eval-exp
         init-env)

(define (apply-primitive-op op args)
  (define (args-error)
    (error 'apply-primitive-op "wrong number of arguments: ~s" args))
  (cond [(eq? op '+) (apply + args)]
        [(eq? op '-) (apply - args)]
        [(eq? op '*) (apply * args)]
        [(eq? op '/) (apply / args)]
        [(eq? op 'add1)
         (cond [(equal? (length args) 1) (add1 (first args))]
               [else (args-error)])]
        [(eq? op 'sub1)
         (cond [(equal? (length args) 1) (sub1 (first args))]
               [else (args-error)])]
        [(eq? op 'negate)
         (cond [(equal? (length args) 1) (* -1 (first args))]
               [else (args-error)])]
        [(eq? op 'list) (apply list args)]
        [(eq? op 'cons)
         (cond [(equal? (length args) 2) (cons (first args) (second args))]
               [else (args-error)])]
        [(eq? op 'car) (car args)]
        [(eq? op 'cdr) (cdr args)]
        [(eq? op 'eqv?) (if (apply equal? args) 'True 'False)]
        [(eq? op 'lt?) (if (apply < args) 'True 'False)]
        [(eq? op 'gt?) (if (apply > args) 'True 'False)]
        [(eq? op 'leq?) (if (apply <= args) 'True 'False)]
        [(eq? op 'geq?) (if (apply >= args) 'True 'False)]
        [else (error 'apply-primitive-op "Unknown primitive: ~s" op)]))

(define (apply-proc proc args)
  (cond [(prim-proc? proc)
         (apply-primitive-op (prim-proc-op proc) args)]
        [(closure? proc) (eval-exp (closure-body proc) (env (closure-params proc) args (closure-env proc)))]
        [else (error 'apply-proc "bad procedure: ~s" proc)]))

(define (duplicator args e)
  (cond [(empty? args) empty]
        [else (cons e (duplicator (rest args) e))]))


(define (eval-exp tree e)
  (cond [(lit-exp? tree) (lit-exp-num tree)]
        [(var-exp? tree) (unbox (env-lookup e (var-exp-symbol tree)))]
        [(app-exp? tree) (apply-proc (eval-exp (app-exp-proc tree) e) (map eval-exp (app-exp-args tree) (duplicator (app-exp-args tree) e)))]
        [(cond-exp? tree) (if (or (equal? (eval-exp (cond-exp-cond tree) e) 0) (equal? (eval-exp (cond-exp-cond tree) e) 'False)) (eval-exp (cond-exp-false tree) e) (eval-exp (cond-exp-true tree) e))]
        [(let-exp? tree) (eval-exp (let-exp-body tree)
                                   (env (let-exp-symbols tree) (map (lambda (binding)
                                                                      (eval-exp binding e))
                                                                    (let-exp-bindings tree))
                                        e))]
        [(lambda-exp? tree) (closure (lambda-exp-param tree) (lambda-exp-body tree) e)]
        [(set-exp? tree) (set-box! (env-lookup e (set-exp-sym tree)) (eval-exp (set-exp-exp tree) e))]
        [(begin-exp? tree) (foldl (lambda (exp acc) (eval-exp exp e)) (void) (begin-exp-exps tree))]
        [else (error 'eval-exp "Invalid tree: ~s" tree)]))

(define primitive-operators '(+ - * / add1 sub1 negate list cons car cdr eqv? lt? gt? leq? geq?))

(define prim-env
  (env primitive-operators
       (map prim-proc primitive-operators)
       empty-env))

(define init-env
  (env '(x y null True False) '(23 42 empty True False) prim-env))

(define (closure parameter-list body environment)
  (list 'closure parameter-list body environment))

(define (closure? obj)
  (if (equal? (first obj) 'closure) #t #f))

(define (closure-params c)
  (second c))

(define (closure-body c)
  (third c))

(define (closure-env c)
  (fourth c))

