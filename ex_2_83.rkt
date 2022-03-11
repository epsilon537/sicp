#lang racket

;Exercise 2.83: Suppose you are designing a generic arithmetic system for dealing with the tower of types shown in Figure 2.25: integer, rational, real, complex. For each type (except
;complex), design a procedure that raises objects of that type one level in the tower. Show how to install a generic raise operation that will work for each type (except complex).

(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

; Original version uses '() instead of false. In MIT scheme '() and #f are the same. In standard scheme they are not, resulting apply-generic to break when given op is not in the table.
(define (get op type)
  (hash-ref *op-table* (list op type) false))

(define *coercion-table* (make-hash))

(define (put-coercion type1 type2 proc)
  (hash-set! *coercion-table* (list type1 type2) proc))

; Original version uses '() instead of false. In MIT scheme '() and #f are the same. In standard scheme they are not, resulting apply-generic to break when given op is not in the table.
(define (get-coercion type1 type2)
  (hash-ref *coercion-table* (list type1 type2) false))

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

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 
                       (get-coercion type1
                                     type2))
                      (t2->t1 
                       (get-coercion type2 
                                     type1)))
                  (cond (t1->t2
                         (apply-generic 
                          op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic 
                          op a1 (t2->t1 a2)))
                        (else
                         (error 
                          "No method for 
                           these types"
                          (list 
                           op 
                           type-tags))))))
              (error 
               "No method for these types"
               (list op type-tags)))))))

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
  
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))

  (put 'make 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))

  (put 'add '(complex complex)
       (lambda (z1 z2) 
         (tag (add-complex z1 z2))))
  
  'done)

(install-complex-package)

(define (make-complex x y)
  ((get 'make 'complex) x y))

(define (install-raise-package)
  (define (raise-real->complex x)
    (make-complex x 0))
  
  (put 'raise '(real)
       raise-real->complex)

  (define (raise-rational->real x)
    (make-real (/ (numer x) (denom x))))
  
  (put 'raise '(rational)
       raise-rational->real)

  (define (raise-integer->rational x)
    (make-rational x 1))
  
  (put 'raise '(integer)
       raise-integer->rational)

  'done)

(install-raise-package)

(define (raise n)
  (apply-generic 'raise n))