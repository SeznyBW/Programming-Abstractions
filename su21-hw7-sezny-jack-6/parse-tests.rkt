#lang racket

(require rackunit rackunit/text-ui rackunit/gui)
(require "parse.rkt")

(provide parse-tests)

(define parse-tests
  (test-suite
   "Parse tests"
   (test-pred "Literal"
           lit-exp?
           (parse 5))
   (test-equal? "lit-exp-num"
                (lit-exp-num (parse 5))
                5)
   (test-pred "variable"
              var-exp?
              (parse 'x))
   (test-equal? "var-exp-symbol"
                (var-exp-symbol (parse 'x))
                'x)
   (test-pred "app-exp"
              app-exp?
              (parse '(+ 1 2 3)))
   (test-equal? "prim-proc"
                (prim-proc-op (app-exp-proc (parse '(+ 1 2 3))))
                '+)
   (test-equal? "literals"
                (map lit-exp-num (app-exp-args (parse '(+ 1 2 3))))
                '(1 2 3))
   (test-exn "bad function"
             exn:fail?
             (lambda () (parse ('foo 1 2 3))))
   (test-exn "bad function with vars"
             exn:fail?
             (lambda () (parse ('foo 1 2 3))))
   (test-exn "empty list"
             exn:fail?
             (lambda () (parse '())))
   (test-pred "cond-exp"
              cond-exp?
              (parse '(if 5 5 0)))
   (test-equal? "cond-exp-cond"
                (cond-exp-cond (parse '(if 1 2 3)))
                '(lit-exp 1))
   (test-equal? "cond-exp-true"
                (cond-exp-true (parse '(if 1 2 3)))
                '(lit-exp 2))
   (test-equal? "cond-exp-false"
                (cond-exp-false (parse '(if 1 2 3)))
                '(lit-exp 3))
   (test-pred "let-exp"
              let-exp?
              (parse '(let ([x 20] [y 5]) (* x y))))
   (test-equal? "let-exp-symbols"
                (let-exp-symbols (parse '(let ([x 20] [y 5]) (* x y))))
                '(x y))
   (test-equal? "let-exp-bindings"
                (let-exp-bindings (parse '(let ([x 20] [y 5]) (* x y))))
                '((lit-exp 20) (lit-exp 5)))
   (test-equal? "let-exp-body"
                (let-exp-body (parse '(let ([x 20] [y 5]) (* x y))))
                '(app-exp (var-exp *) ((var-exp x) (var-exp y))))
   (test-equal? "lambda-exp"
                (parse '((lambda (x y) (+ x y)) 5 6))
                '(app-exp (lambda-exp (x y) (app-exp (var-exp +) ((var-exp x) (var-exp y)))) ((lit-exp 5) (lit-exp 6))))

   (test-equal? "lambda-exp-param"
                (lambda-exp-param '(lambda-exp (x y) (app-exp (var-exp +) ((var-exp x) (var-exp y)))))
                '(x y))
   (test-equal? "lambda-exp-body"
                (lambda-exp-body '(lambda-exp (x y) (app-exp (var-exp +) ((var-exp x) (var-exp y)))))
                '(app-exp (var-exp +) ((var-exp x) (var-exp y))))
   (test-equal? "set!"
                (parse '(set! y 10))
                '(set-exp y (lit-exp 10)))
   (test-equal? "begin"
                (parse '(begin (set! x 8) (+ x 5)))
                '(begin-exp ((set-exp x (lit-exp 8)) (app-exp (var-exp +) ((var-exp x) (lit-exp 5))))))
   ))
