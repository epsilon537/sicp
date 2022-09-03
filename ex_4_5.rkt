#lang racket

(define ns (make-base-namespace))

;Exercise 4.5: Scheme allows an additional syntax for cond clauses, (⟨test⟩ => ⟨recipient⟩). If ⟨test⟩ evaluates to a true value, then ⟨recipient⟩ is evaluated. Its value must be a
;procedure of one argument; this procedure is then invoked on the value of the ⟨test⟩, and the result is returned as the value of the cond expression. For example
;
;(cond ((assoc 'b '((a 1) (b 2))) => cadr)
;      (else false))
;
;returns 2. Modify the handling of cond so that it supports this extended syntax.

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate 
                 consequent 
                 alternative)
  (list 'if 
        predicate 
        consequent 
        alternative))

(define (begin? exp) 
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (cond? exp) 
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-arrow-clause? clause)
  (eq? (cadr clause) '=>))
(define (cond-predicate clause) 
  (car clause))
(define (cond-actions clause)
      (cdr clause))
(define (cond-clause-recipient clause)
  (eval (caddr clause) ns))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp 
                 (cond-actions first))
                (error "ELSE clause isn't 
                        last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (if (cond-arrow-clause? first)
                         ((cond-clause-recipient first) (eval (cond-predicate first) ns))
                         (sequence->exp 
                          (cond-actions first)))
                     (expand-clauses 
                      rest))))))

(cond? '(cond ((assoc 'b '((a 1) (b 2))) => cadr)
      (else false)))

(eval (cond->if '(cond ((assoc 'b '((a 1) (b 2))) => cadr)
                       (else false))) ns)