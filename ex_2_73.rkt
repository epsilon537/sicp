#lang racket
;Using lang racket instead of sicp so the make-hash function becomes available.

;Exercise 2.73: 2.3.2 described a program that performs symbolic differentiation:
;
;(define (deriv exp var)
;  (cond ((number? exp) 0)
;        ((variable? exp) 
;         (if (same-variable? exp var) 1 0))
;        ((sum? exp)
;         (make-sum (deriv (addend exp) var)
;                   (deriv (augend exp) var)))
;        ((product? exp)
;         (make-sum
;           (make-product 
;            (multiplier exp)
;            (deriv (multiplicand exp) var))
;           (make-product 
;            (deriv (multiplier exp) var)
;            (multiplicand exp))))
;        ⟨more rules can be added here⟩
;        (else (error "unknown expression type:
;                      DERIV" exp))))
;
;We can regard this program as performing a dispatch on the type of the expression to be differentiated. In this situation the “type tag” of the datum is the algebraic operator symbol
;(such as +) and the operation being performed is deriv. We can transform this program into data-directed style by rewriting the basic derivative procedure as
;
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) 
           (if (same-variable? exp var) 
               1 
               0))
         (else ((get 'deriv (operator exp)) 
                (operands exp) 
                var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;    Explain what was done above. 
;
;    The above is using an extensible data-directed implementation. Expressions are expected to start with an operator, followed by operands. The operator is the 'tag' by which the
;    table is indexed. This allows developers to add derivation functionality for new operators without any impact on the generic deriv function.
;    number? and variable? are excluded from the table-based dispatch because they don't follow the general expression syntax which starts with an operator and is followed by operands.
;
;    Why can’t we assimilate the predicates number? and variable? into the data-directed dispatch?

;    Derivation of an expression is implemented in terms of derivations of it sub-expressions. This eventually has to reduce to primitive numbers and variables (instead of user-defined
;    number or variable types that follow the operator-operand expression syntax). The implementation of the various operation derivation method count on that.
;    There's also not much point in doing so since the behavior of derivation of numbers and variables is known/fixed, and doesn't need to be pluggable/extensible.
;    If we do represent numbers as numbers and variables as variables, these don't follow the general expression syntax. The operator and operands
;    accessors would fail on them, so they have to be special-cased.

;    Write the procedures for derivatives of sums and products, and the auxiliary code required to install them in the table used by the program above.

; Defining get and put in Racket:

(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (install-deriv-sum-and-mul-package)
  (define (sum? x)
    (and (pair? x) (eq? (car x) '+)))
  (define (addend s) (cadr s))
  (define (augend s) (caddr s))
  (define (product? x)
    (and (pair? x) (eq? (car x) '*)))
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (caddr p))
  (define (=number? exp num)
    (and (number? exp) (= exp num)))

  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) 
           (+ a1 a2))
          (else (list '+ a1 a2))))

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) 
               (=number? m2 0)) 
           0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) 
           (* m1 m2))
          (else (list '* m1 m2))))

  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  (define (deriv-mul exp var)
    (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))

  (put 'deriv '* (lambda (opernds vr) (deriv-mul (cons '* opernds) vr))) ;Using these lambdas so I can re-use the code from ex.2.56 as-is.
  (put 'deriv '+ (lambda (opernds vr) (deriv-sum (cons '+ opernds) vr)))
  'done
  )

(install-deriv-sum-and-mul-package)

;    Choose any additional differentiation rule that you like, such as the one for exponents (Exercise 2.56), and install it in this data-directed system.
(define (install-deriv-exp-package)
  (define (sum? x)
    (and (pair? x) (eq? (car x) '+)))
  (define (addend s) (cadr s))
  (define (augend s) (caddr s))
  (define (product? x)
    (and (pair? x) (eq? (car x) '*)))
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (caddr p))
  (define (=number? exp num)
    (and (number? exp) (= exp num)))

  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) 
           (+ a1 a2))
          (else (list '+ a1 a2))))

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) 
               (=number? m2 0)) 
           0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) 
           (* m1 m2))
          (else (list '* m1 m2))))

  (define (exponentiation? x)
   (and (pair? x) (eq? (car x) '**)))

  (define (base e)
    (cadr e))

  (define (exponent e)
    (caddr e))

  (define (make-exponentiation b e)
    (cond ((=number? e 0) 
           1)
          ((=number? e 1)
           b)
          (else (list '** b e))))

  (define (dec x)
    (- x 1))
  
  (define (deriv-exp exp var)
    (make-product
     (make-product
      (exponent exp)
      (make-exponentiation (base exp) (dec (exponent exp))))
     (deriv (base exp) var)))

  (put 'deriv '** (lambda (opernds vr) (deriv-exp (cons '* opernds) vr))) ;Using this lambda so I can re-use the code from ex.2.56 as-is.
  'done
  )

(install-deriv-exp-package)
