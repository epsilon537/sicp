#lang racket

;Exercise 2.84: Using the raise operation of Exercise 2.83, modify the apply-generic procedure so that it coerces its arguments to have the same type by the method of successive raising,
;as discussed in this section. You will need to devise a way to test which of two types is higher in the tower. Do this in a manner that is “compatible” with the rest of the system and
;will not lead to problems in adding new levels to the tower.

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

(define tower '(integer rational real complex))

(define (raise n)
  (let ((raise-fun (get 'raise (type-tag n))))
    (if raise-fun
        (raise-fun n)
        (error "No raise function defined for type: " (type-tag n)))))

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
          (apply proc (map contents args))
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