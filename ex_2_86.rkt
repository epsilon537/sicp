#lang racket

;Exercise 2.86: Suppose we want to handle complex numbers whose real parts, imaginary parts, magnitudes, and angles can be either ordinary numbers, rational numbers, or other numbers we
;might wish to add to the system. Describe and implement the changes to the system needed to accommodate this. You will have to define operations such as sine and cosine that are generic
;over ordinary numbers and rational numbers.

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
      'real))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      datum))

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
          (cond ((= (length args) 1)
                 (apply-generic op (raise (car args))))
                ((= (length args) 2)
                 (let ((type1 (car type-tags))
                       (type2 (cadr type-tags))
                       (a1 (car args))
                       (a2 (cadr args)))
                   (if (eq? (lowest-type type1 type2) type1)
                       (apply-generic op (raise a1) a2)
                       (apply-generic op a1 (raise a2)))))
                (else
                 (error "No method for these types" (list op type-tags))))))))
                    
(define (add x y) (apply-generic 'add x y))
(define (mul x y) (apply-generic 'mul x y))
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (arctan x y) (apply-generic 'arctan x y))
(define (squareroot x) (apply-generic 'squareroot x))
(define (square x) (mul x x))

(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  
  (put 'make 'integer
       (lambda (x) (tag x)))

  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))

  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  
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

  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))

  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  
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
       (lambda (x) (tag x)))

  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))

  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))

  (put 'sine '(real)
       (lambda (x) (tag (sin x))))

  (put 'cosine '(real)
       (lambda (x) (tag (cos x))))

  (put 'arctan '(real real)
       (lambda (x y) (tag (atan x y))))

  (put 'squareroot '(real)
       (lambda (x) (tag (sqrt x))))
  
  'done)

(install-real-package)

(define (make-real n)
  ((get 'make 'real) n))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) 
    (cons x y))
  (define (magnitude z)
    (squareroot (add (square (real-part z))
                     (square (imag-part z)))))
  (define (angle z)
    (arctan (imag-part z) (real-part z)))
  ;(define (make-from-mag-ang r a)
  ;  (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) 
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  ;(put 'make-from-mag-ang 'rectangular
  ;     (lambda (r a) 
  ;       (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  ;(define (make-from-real-imag x y)
  ;  (cons (sqrt (+ (square x) (square y)))
  ;        (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  ;(put 'make-from-real-imag 'polar
  ;     (lambda (x y) 
  ;       (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

(install-polar-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (real-part z) 
  (apply-generic 'real-part z))
(define (imag-part z) 
  (apply-generic 'imag-part z))
(define (magnitude z) 
  (apply-generic 'magnitude z))
(define (angle z) 
  (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 
        'rectangular) 
   x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 
        'polar) 
   r a))

(define (install-complex-package)
  ;; imported procedures from rectangular 
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 
          'rectangular) 
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) 
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag 
     (add (real-part z1) (real-part z2))
     (add (imag-part z1) (imag-part z2))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))

  (put 'add '(complex complex)
       (lambda (z1 z2) 
         (tag (add-complex z1 z2))))

  (put 'make-from-real-imag 'complex
       (lambda (x y) 
         (tag (make-from-real-imag x y))))

  (put 'make-from-mag-ang 'complex
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  
  'done)

(install-complex-package)

(define (install-raise-package)
  (define (raise-real->complex x)
    (make-complex-from-real-imag (contents x) 0))
  
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
    (and (equ? (real-part (tag-complex x)) (real-part (tag-complex y))) (equ? (imag-part (tag-complex x)) (imag-part (tag-complex y)))))

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