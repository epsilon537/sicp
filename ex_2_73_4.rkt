#lang racket
;Using lang racket instead of sicp so the make-hash function becomes available.

;    In this simple algebraic manipulator the type of an expression is the algebraic operator that binds it together. Suppose, however, we indexed the procedures in the opposite way, so
;    that the dispatch line in deriv looked like
;
;    ((get (operator exp) 'deriv) 
;     (operands exp) var)
;
;    What corresponding changes to the derivative system are required?

; This is like swapping row and column in a 2D table. Both operator and 'deriv are table keys. Put operation have to be done with the keys in the same order.

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) 
           (if (same-variable? exp var) 
               1 
               0))
         (else ((get (operator exp) 'deriv) 
                (operands exp) 
                var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

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

  (put '* 'deriv (lambda (opernds vr) (deriv-mul (cons '* opernds) vr))) ;Using these lambdas so I can re-use the code from ex.2.56 as-is.
  (put '+ 'deriv (lambda (opernds vr) (deriv-sum (cons '+ opernds) vr)))
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

  (put '** 'deriv (lambda (opernds vr) (deriv-exp (cons '* opernds) vr))) ;Using this lambda so I can re-use the code from ex.2.56 as-is.
  'done
  )

(install-deriv-exp-package)

