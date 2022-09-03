#lang racket

;Exercise 4.7: Let* is similar to let, except that the bindings of the let* variables are performed sequentially from left to right, and each binding is made in an environment in ;which all of the preceding bindings are visible. For example
;
;(let* ((x 3)
;       (y (+ x 2))
;       (z (+ x y 5)))
;  (* x z))
;
;returns 39. Explain how a let* expression can be rewritten as a set of nested let expressions, and write a procedure let*->nested-lets that performs this transformation. If we
;have already implemented let (Exercise 4.6) and we want to extend the evaluator to handle let*, is it sufficient to add a clause to eval whose action is
;
;(eval (let*->nested-lets exp) env)
;
;or must we explicitly expand let* in terms of non-derived expressions?
;
;A: Straightforward nesting of let blocks: A nested block's environment points to the enveloping block's environment so preceding bindings are visible.

(define ns (make-base-namespace))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (lambda? exp) 
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (let? exp) 
  (tagged-list? exp 'let))
(define (let-clauses exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-var clause) (car clause))
(define (let-exp clause) (cadr clause))

(define (let->combination exp)
  (cons (make-lambda (map let-var (let-clauses exp)) (let-body exp))
        (map let-exp (let-clauses exp))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (lookup-variable-value var env)
  (error "TBD"))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) 
  (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (eval-assignment exp env)
  (set-variable-value! 
   (assignment-variable exp)
   (eval (assignment-value exp) env)
   env)
  'ok)

(define (eval-definition exp env)
  (define-variable! 
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (set-variable-value! var val env)
  (error "TBD"))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! 
              var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda 
       (cdadr exp)   ; formal parameters
       (cddr exp)))) ; body

(define (enclosing-environment env) (error "TBD"))
(define (first-frame env)  (error "TBD"))
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (error "TBD"))
(define (frame-values frame) (error "TBD"))
(define (add-binding-to-frame! var val frame)
  (error "TBD"))

(define (set-car! a b) (error "TBD"))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (true? x)
  (error "TBD"))

(define (false? x)
  (error "TBD"))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (begin? exp) 
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) 
         (eval (first-exp exps) env))
        (else 
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) 
                        env))))

(define (cond? exp) 
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) 
  (car clause))
(define (cond-actions clause) 
  (cdr clause))
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
                     (sequence->exp 
                      (cond-actions first))
                     (expand-clauses 
                      rest))))))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (make-if predicate 
                 consequent 
                 alternative)
  (list 'if 
        predicate 
        consequent 
        alternative))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values 
             (rest-operands exps) 
             env))))

(define (primitive-procedure? proc)
  (error "TBD"))

(define (primitive-implementation proc) 
  (error "TBD"))

(define (apply-primitive-procedure proc args)
  (error "TBD"))

(define (extend-environment vars vals base-env)
  (error "TBD"))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure 
          procedure 
          arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters 
              procedure)
             arguments
             (procedure-environment 
              procedure))))
        (else
         (error "Unknown procedure 
                 type: APPLY" 
                procedure))))

(define (make-let clauses body)
  (cons 'let (cons (list clauses) body)))

(define (let*? exp) 
  (tagged-list? exp 'let))

(define (let*->nested-lets exp)
  (define (nested-lets clauses body)
    (make-let (car clauses)
              (if (empty? (cdr clauses))
                  body
                  (list (nested-lets (cdr clauses) body)))))
  (nested-lets (let-clauses exp) (let-body exp)))

(define (eval exp env)
  (cond ((self-evaluating? exp) 
         exp)
        ((variable? exp) 
         (lookup-variable-value exp env))
        ((quoted? exp) 
         (text-of-quotation exp))
        ((assignment? exp) 
         (eval-assignment exp env))
        ((definition? exp) 
         (eval-definition exp env))
        ((if? exp) 
         (eval-if exp env))
        ((lambda? exp)
         (make-procedure 
          (lambda-parameters exp)
          (lambda-body exp)
          env))
        ((begin? exp)
         (eval-sequence 
          (begin-actions exp) 
          env))
        ((let? exp)
         (eval (let->combination exp) env))
        ((let*? exp)
         (eval (let*->nested-lets exp) env))
        ((cond? exp) 
         (eval (cond->if exp) env))
        ((application? exp)
         (display "apply: ")
         (display exp)
         (newline)
         (apply (eval (operator exp) env)
                (list-of-values 
                 (operands exp) 
                 env)))
        (else
         (error "Unknown expression 
                 type: EVAL" exp))))

(let*->nested-lets 
 '(let ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z)))
       