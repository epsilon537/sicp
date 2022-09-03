#lang racket
;Exercise 4.10: By using data abstraction, we were able to write an eval procedure that is independent of the particular syntax of the language to be evaluated. To illustrate
;this, design and implement a new syntax for Scheme by modifying the procedures in this section, without changing eval or apply.

(define ns (make-base-namespace))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (lambda? exp) 
  (tagged-list? exp 'anonymous)) ;<- lambda->anonymous
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'anonymous (cons parameters body)))

(define (let? exp) 
  (tagged-list? exp 'bind)) ;<- let->bind
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

(define (assignment? exp) ;<- set!-> x=y
  (and (pair? exp) (eq? (cadr exp) '=)))

(define (assignment-variable exp) 
  (car exp))

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

;(if pred conseq seq else alt seq endif)

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp)
  (define (accumulate-until-else-endif conseq)
    (if (or (eq? (car conseq) 'else) (eq? (car conseq) 'endif))
        '()
        (cons (car conseq) (accumulate-until-else-endif (cdr conseq)))))
  (accumulate-until-else-endif (cddr exp)))
(define (if-alternative exp)
  (define (accumulate-until-endif conseq)
    (if (eq? (car conseq) 'endif)
        '()
        (cons (car conseq) (accumulate-until-endif (cdr conseq)))))
  (define (accumulate-until-else-endif conseq)
    (cond ((eq? (car conseq) 'else)
           (accumulate-until-endif (cdr conseq)))
          ((eq? (car conseq) 'endif)
           'false)
          (else (accumulate-until-else-endif (cdr conseq)))))
  (accumulate-until-else-endif (cddr exp)))
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
        'else
        alternative
        'endif))

(define (application? exp)
  (pair? exp))
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

(lambda? '(anonymous (x) (* x 2)))
(lambda-parameters '(anonymous (x) (* x 2)))
(lambda-body '(anonymous (x) (* x 2)))

(assignment? '(x = 2))
(assignment-variable '(x = 2))
(assignment-value '(x = 2))

(if? '(if (> 2 1) (display "true") (newline) else (display "false") (newline) endif))
(if-consequent '(if (> 2 1) (display "true") (newline) else (display "false") (newline) endif))
(if-alternative '(if (> 2 1) (display "true") (newline) else (display "false") (newline) endif))
