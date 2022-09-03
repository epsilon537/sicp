#lang racket

;Exercise 4.4: Recall the definitions of the special forms and and or from Chapter 1:
;
;    and: The expressions are evaluated from left to right. If any expression evaluates to false, false is returned; any remaining expressions are not evaluated. If all the
;expressions evaluate to true values, the value of the last expression is returned. If there are no expressions then true is returned.
;    or: The expressions are evaluated from left to right. If any expression evaluates to a true value, that value is returned; any remaining expressions are not evaluated. If
;all expressions evaluate to false, or if there are no expressions, then false is returned. 
;
;Install and and or as new special forms for the evaluator by defining appropriate syntax procedures and evaluation procedures eval-and and eval-or. Alternatively, show how to
;implement and and or as derived expressions.

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

(define (make-and-exp ops) (cons 'and ops))
(define (make-or-exp ops) (cons 'or ops))

(define (operands exp) (cdr exp))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (eval-and exp)
  (let ((ops (operands exp)))
    (if (null? ops)
        #t
        (let ((first (first-operand ops))
              (rest (rest-operands ops)))
          (if first
              (if (null? rest)
                  first
                  (eval-and (make-and-exp rest)))
              #f)))))

(define (eval-or exp)
  (let ((ops (operands exp)))
    (if (null? ops)
        #f
        (let ((first (first-operand ops)))
          (if first
              first
              (eval-or (make-or-exp (rest-operands ops))))))))

(and? '(and 1 #t 3))
(eval-and '(and 1 #t 3))
(eval-and '(and 1 #f 3))

(or? '(or #f 2 #f))
(eval-or '(or #f 2 #f))