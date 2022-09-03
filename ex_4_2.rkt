#lang racket

;Exercise 4.2: Louis Reasoner plans to reorder the cond clauses in eval so that the clause for procedure applications appears before the clause for assignments. He argues that
;this will make the interpreter more efficient: Since programs usually contain more applications than assignments, definitions, and so on, his modified eval will usually check
;fewer clauses than the original eval before identifying the type of an expression.
;
;    What is wrong with Louis’s plan? (Hint: What will Louis’s evaluator do with the expression (define x 3)?)
;    Louis is upset that his plan didn’t work. He is willing to go to any lengths to make his evaluator recognize procedure applications before it checks for most other kinds of
;expressions. Help him by changing the syntax of the evaluated language so that procedure applications start with call. For example, instead of (factorial 3) we will now have to
;write (call factorial 3) and instead of (+ 1 2) we will have to write (call + 1 2).

;A: application? is a catch-all. It assumes that all other expression classification tests have been tried and failed. If you move it up in the evaluator's cond it will catch all pairs. Downstream cases such as 'define' would not get tried. (define x 3) would be classified as a procedure application.

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (application? exp)
  (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(application? '(call + 1 2))
(operator '(call + 1 2))
(operands '(call + 1 2))
(first-operand (operands '(call + 1 2)))
(rest-operands (operands '(call + 1 2)))