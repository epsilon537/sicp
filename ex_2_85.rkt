#lang racket

;Exercise 2.85: This section mentioned a method for “simplifying” a data object by lowering it in the tower of types as far as possible. Design a procedure drop that accomplishes this
;for the tower described in Exercise 2.83. The key is to decide, in some general way, whether an object can be lowered. For example, the complex number 1.5 + 0 i can be lowered as far as
;real, the complex number 1 + 0 i can be lowered as far as integer, and the complex number 2 + 3 i cannot be lowered at all. Here is a plan for determining whether an object can be
;lowered: Begin by defining a generic operation project that “pushes” an object down in the tower. For example, projecting a complex number would involve throwing away the imaginary
;part. Then a number can be dropped if, when we project it and raise the result back to the type we started with, we end up with something equal to what we started with. Show how to
;implement this idea in detail, by writing a drop procedure that drops an object as far as possible. You will need to design the various projection operations119 and install project as a
;generic operation in the system. You will also need to make use of a generic equality predicate, such as described in Exercise 2.79. Finally, use drop to rewrite apply-generic from
;Exercise 2.84 so that it “simplifies” its answers.

(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

; Original version uses '() instead of false. In MIT scheme '() and #f are the same. In standard scheme they are not, resulting apply-generic to break when given op is not in the table.
(define (get op type)
  (hash-ref *op-table* (list op type) false))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: 
              TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: 
              CONTENTS" datum)))

(define (tagged? datum)
  (and (pair? datum) (not (empty? (filter (lambda (x) (eq? x (type-tag datum))) tower)))))

(define tower '(integer rational real complex))

(define (raise n)
  (let ((raise-fun (get 'raise (type-tag n))))
    (if raise-fun
        (raise-fun n)
        (error "No raise function defined for type: " (type-tag n)))))

(define (project n)
  (let ((project-fun (get 'project (type-tag n))))
    (if project-fun
        (project-fun n)
        (error "No project function defined for type: " (type-tag n)))))

(define (drop n)
  (let ((project-fun (get 'project (type-tag n))))
    (if project-fun
        (let ((dropped-n (project-fun n)))
          (let ((raise-fun (get 'raise (type-tag dropped-n))))
            (if raise-fun
                (let ((reraised-n (raise-fun dropped-n)))
                  (if (equ? reraised-n n)
                      (drop dropped-n)
                      n))
                n)))
        n)))
        

(define (lowest-type t1 t2)
  (define (lowest-type-loop twr)
    (cond ((empty? twr) (error "type not found in tower"))
          ((eq? (car twr) t1) t1)
          ((eq? (car twr) t2) t2)
          (else (lowest-type-loop (cdr twr)))))
  (lowest-type-loop tower))
                        
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (let ((apply-res (apply proc (map contents args))))
            (if (tagged? apply-res)
                (drop apply-res)
                apply-res))
          (if (not (= (length args) 2))
              (error "No method for these types" (list op type-tags))
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? (lowest-type type1 type2) type1)
                    (apply-generic op (raise a1) a2)
                    (apply-generic op a1 (raise a2)))))))))
                    
(define (add x y) (apply-generic 'add x y))

(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  
  (put 'make 'integer
       (lambda (x) (tag x)))

  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  
  'done)

(install-integer-package)

(define (make-integer n)
  ((get 'make 'integer) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))

  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  
  'done)

(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (numer n)
  (apply-generic 'numer n))

(define (denom n)
  (apply-generic 'denom n))

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))

  (put 'make 'real
       (lambda (x) (tag (* 1.0 x))))

  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  
  'done)

(install-real-package)

(define (make-real n)
  ((get 'make 'real) n))

(define (install-complex-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) 
    (cons x y))

  (define (add-complex z1 z2)
    (make-from-real-imag 
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  
  (define (tag z) (attach-tag 'complex z))

  (put 'make 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))

  (put 'add '(complex complex)
       (lambda (z1 z2) 
         (tag (add-complex z1 z2))))

  (put 'real-part '(complex) real-part)

  (put 'imag-part '(complex) imag-part)
  
  'done)

(install-complex-package)

(define (make-complex x y)
  ((get 'make 'complex) x y))

(define (real-part z) 
  (apply-generic 'real-part z))

(define (imag-part z) 
  (apply-generic 'imag-part z))

(define (install-raise-package)
  (define (raise-real->complex x)
    (make-complex (contents x) 0))
  
  (put 'raise 'real raise-real->complex)

  (define (raise-rational->real x)
    (make-real (/ (numer x) (denom x))))
  
  (put 'raise 'rational raise-rational->real)

  (define (raise-integer->rational x)
    (make-rational (contents x) 1))
  
  (put 'raise 'integer raise-integer->rational)

  'done)

(install-raise-package)

(define (install-project-package)
  (define (project-complex->real x)
    (make-real (real-part x)))

  (put 'project 'complex project-complex->real)

  (define (project-real->rational x)
    (make-rational (round (contents x)) 1))

  (put 'project 'real project-real->rational)

  (define (project-rational->integer x)
    (make-integer (round (/ (numer x) (denom x)))))

  (put 'project 'rational project-rational->integer)
  
  'done)

(install-project-package)

(define (install-equ-package)
  (define (tag-rat x)
    (cons 'rational x))

  (define (tag-complex x)
    (cons 'complex x))
  
  (define (equ-rat? x y)
    (and (= (numer (tag-rat x)) (numer (tag-rat y))) (= (denom (tag-rat x)) (denom (tag-rat y)))))

  (define (equ-complex? x y)
    (and (= (real-part (tag-complex x)) (real-part (tag-complex y))) (= (imag-part (tag-complex x)) (imag-part (tag-complex y)))))

  (define (equ-number? x y)
    (= x y))
  
  (put 'equ? '(rational rational)
       (lambda (x y) (equ-rat? x y)))
  (put 'equ? '(complex complex)
       (lambda (x y) (equ-complex? x y)))
  (put 'equ? '(integer integer)
       (lambda (x y) (equ-number? x y)))
  (put 'equ? '(real real)
       (lambda (x y) (equ-number? x y)))
  'done)

(install-equ-package)

(define (equ? x y) (apply-generic 'equ? x y))