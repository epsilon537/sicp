#lang racket

;Exercise 4.1: Notice that we cannot tell whether the metacircular evaluator evaluates operands from left to right or from right to left. Its evaluation order is inherited from
;the underlying Lisp: If the arguments to cons in list-of-values are evaluated from left to right, then list-of-values will evaluate operands from left to right; and if the
;arguments to cons are evaluated from right to left, then list-of-values will evaluate operands from right to left.
;
;Write a version of list-of-values that evaluates operands from left to right regardless of the order of evaluation in the underlying Lisp. Also write a version of list-of-values
;that evaluates operands from right to left.

(define no-operands? empty?)
(define first-operand car)
(define rest-operands cdr)
(define (eval x env) x)

(define (list-of-values-left-to-right exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
        (let ((rest (list-of-values-left-to-right 
                     (rest-operands exps) 
                     env)))
      (cons first rest)))))

(list-of-values-left-to-right (list 1 2 3 4) '())

(define (list-of-values-right-to-left exps env)
  (reverse (list-of-values-left-to-right exps env)))

(list-of-values-right-to-left (list 1 2 3 4) '())

  