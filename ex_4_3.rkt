#lang racket

;Exercise 4.3: Rewrite eval so that the dispatch is done in data-directed style. Compare this with the data-directed differentiation procedure of Exercise 2.73. (You may use the ;car of a compound expression as the type of the expression, as is appropriate for the syntax implemented in this section.)

(define *op-table* (make-hash))

(define (put op proc)
  (hash-set! *op-table* op proc))

(define (get op)
  (hash-ref *op-table* op '()))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (lookup-variable-value exp env)
  (error "TBD"))

(define (set-variable-value! var val env)
  (error "TBD"))

(define (define-variable! var val env)
  (error "TBD"))

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

(define (primitive-procedure? proc)
 (error "TBD"))

(define (apply-primitive-procedure proc args)
  (error "TBD"))

(define (compound-procedure? p)
  (error "TBD"))

(define (make-procedure parameters body env)
  (error "TBD"))

(define (procedure-body p)
  (error "TBD"))

(define (procedure-parameters p) (error "TBD"))

(define (procedure-environment p) (error "TBD"))

(define (extend-environment vars vals base-env)
  (error "TBD"))

(define (true? x)
  (error "TBD"))
(define (false? x)
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

(define (eval exp env)
  (cond ((self-evaluating? exp) 
         exp)
        ((variable? exp) 
         (lookup-variable-value exp env))
        (else
         (if (pair? exp)
             (let ((op (get (car exp))))
               (if (empty? op)
                   (apply (eval (operator exp) env)
                          (list-of-values 
                           (operands exp) 
                           env))
                   (op exp env)))
             (error "Unknown expression type: EVAL" exp)))))

(define (install-eval-ops)
  (define (text-of-quotation exp)
    (cadr exp))

  (define (assignment-variable exp) 
  (cadr exp))

  (define (assignment-value exp) (caddr exp))

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

  (define (lambda-parameters exp) (cadr exp))
  (define (lambda-body exp) (cddr exp))
  (define (make-lambda parameters body)
    (cons 'lambda (cons parameters body)))

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

  (define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else (make-begin seq))))

  (define (make-begin seq) (cons 'begin seq))

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

  (define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))

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
  
  (put 'quote text-of-quotation)
  (put 'set! eval-assignment)
  (put 'define eval-definition)
  (put 'if eval-if)
  (put 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp) (lambda-body exp) env)))
  (put 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
  (put 'cond (lambda (exp env) (eval (cond->if exp) env)))
  )

(install-eval-ops)

;This is similar to deriv in ex_2_73. Special cases are handled in the cond, the rest is data-driven. The op table is simpler here, just a single dispatch based on the op ;symbol.
