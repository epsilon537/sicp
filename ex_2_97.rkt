#lang racket

;Exercise 2.97:

;Implement this algorithm as a procedure reduce-terms that takes two term lists n and d as arguments and returns a list nn, dd, which are n and d reduced to lowest terms via the
;algorithm given above. Also write a procedure reduce-poly, analogous to add-poly, that checks to see if the two polys have the same variable. If so, reduce-poly strips off the variable
;and passes the problem to reduce-terms, then reattaches the variable to the two term lists supplied by reduce-terms.
;Define a procedure analogous to reduce-terms that does what the original make-rat did for integers:
;
;    (define (reduce-integers n d)
;      (let ((g (gcd n d)))
;        (list (/ n g) (/ d g))))
;
;and define reduce as a generic operation that calls apply-generic to dispatch to either reduce-poly (for polynomial arguments) or reduce-integers (for scheme-number arguments). You can
;now easily make the rational-arithmetic package reduce fractions to lowest terms by having make-rat call reduce before combining the given numerator and denominator to form a rational
;number. The system now handles rational expressions in either integers or polynomials. To test your program, try the example at the beginning of this extended exercise:
;
;    (define p1 
;      (make-polynomial 'x '((1 1) (0 1))))
;    (define p2 
;      (make-polynomial 'x '((3 1) (0 -1))))
;    (define p3 
;      (make-polynomial 'x '((1 1))))
;    (define p4 
;      (make-polynomial 'x '((2 1) (0 -1))))
;    (define rf1 (make-rational p1 p2))
;    (define rf2 (make-rational p3 p4))
;    (add rf1 rf2)
;
;    See if you get the correct answer, correctly reduced to lowest terms.

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

(define (square x) (* x x))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (add-complex z1 z2)
  (make-from-real-imag 
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag 
   (- (real-part z1) (real-part z2))
   (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang 
   (* (magnitude z1) (magnitude z2))
   (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang 
   (/ (magnitude z1) (magnitude z2))
   (- (angle z1) (angle z2))))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (if (number? datum)
      'scheme-number
      (if (pair? datum)
          (car datum)
          (error "Bad tagged datum: 
              TYPE-TAG" datum))))

(define (contents datum)
  (if (number? datum)
      datum
      (if (pair? datum)
          (cdr datum)
          (error "Bad tagged datum: 
              CONTENTS" datum))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (define (err-msg)
      (error "No method for these types" (list op type-tags)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (err-msg)
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic 
                              op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic 
                              op a1 (t2->t1 a2)))
                            (else
                             (err-msg))))))
                (err-msg))))))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) 
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
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
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

(install-polar-package)

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

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (reduce x y) (apply-generic 'reduce x y))
(define (greatest-common-divisor x y) (apply-generic 'greatest-common-divisor x y))

(define (install-scheme-number-package)
  (define (reduce-integers n d)
    (let ((g (gcd n d)))
      (list (/ n g) (/ d g))))
  
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'reduce '(scheme-number scheme-number)
        (lambda (x y) (map tag (reduce-integers x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  ;; following added to Scheme-number package
  (put 'exp 
       '(scheme-number scheme-number)
       (lambda (x y) 
         (tag (expt x y)))) ; using primitive expt

  (put 'greatest-common-divisor '(scheme-number scheme-number)
       (lambda (x y) (tag (gcd x y))))
  
  'done)

(install-scheme-number-package)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  
  (define (make-rat n d)
    (let ((n-d-reduced (reduce n d)))
      (apply cons n-d-reduced)))
    
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  'done)

(install-rational-package)

(define (numer n)
  (apply-generic 'numer n))

(define (denom n)
  (apply-generic 'denom n))

(define (make-rational n d)
  ((get 'make 'rational) n d))

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
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag 
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang 
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang 
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) 
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) 
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) 
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) 
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

(define (install-zero-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  
  (define (=zero-rat? x)
    (= (numer x) 0))

  (define (=zero-complex? x)
    (and (= (real-part x) 0) (= (imag-part x) 0)))

  (define (=zero-number? x)
    (= x 0))
  
  (put '=zero? '(rational)
       (lambda (x) (=zero-rat? x)))
  (put '=zero? '(complex)
       (lambda (x) (=zero-complex? x)))
  (put '=zero? '(scheme-number)
       (lambda (x) (=zero-number? x)))
  'done)

(install-zero-package)

(define (=zero? x) (apply-generic '=zero? x))

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)

(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)

(put-coercion 'complex 'complex 
              complex->complex)

(define (exp x y) 
  (apply-generic 'exp x y))

(define (make-term order coeff) 
  (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) 
  (null? term-list))

(define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

(define (make-term-list terms)
    (if (empty? terms)
        '()
        (adjoin-term (car terms) (make-term-list (cdr terms)))))

(define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) 
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 
                     (add-terms (rest-terms L1) 
                                L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 
                     (add-terms 
                      L1 
                      (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term 
                      (order t1)
                      (add (coeff t1) 
                           (coeff t2)))
                     (add-terms 
                      (rest-terms L1)
                      (rest-terms L2)))))))))

(define (neg-term-list tl)
  (if (empty-termlist? tl)
      '()
      (let ((neg-c (sub 0 (coeff (first-term tl))))
            (ord (order (first-term tl))))
        (adjoin-term (make-term ord neg-c) (neg-term-list (rest-terms tl))))))

(define (sub-terms t1 t2)
  (add-terms t1 (neg-term-list t2)))

(define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms 
         (mul-term-by-all-terms 
          (first-term L1) L2)
         (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term 
          (+ (order t1) (order t2))
          (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms 
          t1 
          (rest-terms L))))))

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) 
            (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) 
                              (coeff t2)))
                  (new-o (- (order t1) 
                            (order t2))))
              (let ((rest-of-result (div-terms
                                     (sub-terms L1
                                                (mul-terms (make-term-list (list (make-term new-o new-c)))
                                                           L2))
                                     L2)))
                (list (adjoin-term (make-term new-o new-c) (car rest-of-result))
                      (cadr rest-of-result))))))))

(define (remainder-terms a b)
  (cadr (div-terms a b)))

(define (pseudoremainder-terms a b)
  (let ((o1 (order (first-term a)))
        (o2 (order (first-term b)))
        (c (coeff (first-term b))))
    (if (not (number? c))
        (error "coeff not a number: " c)
        (let ((integerizing-factor (exp c (+ 1 o1 (- o2)))))
          (remainder-terms (mul-terms (make-term-list (list (make-term 0 integerizing-factor)))
                                      a)
                           b)))))

(define (gcd-terms a b)
  (define (gcd-terms-iter a b)
    (if (empty-termlist? b)
      a
      (gcd-terms-iter b (pseudoremainder-terms a b))))
  (let ((terms-before-reduction (gcd-terms-iter a b)))
    (let ((g (foldr gcd
                    (coeff (first-term terms-before-reduction))
                    (map coeff terms-before-reduction))))
      (car (div-terms terms-before-reduction
                 (make-term-list (list (make-term 0 g))))))))

(define (reduce-terms n d)
  (let ((g (gcd-terms n d)))
    (let ((o1 (max (order (first-term n)) (order (first-term d))))
          (o2 (order (first-term g)))
          (c (coeff (first-term g))))
      (let ((integerizing-factor (exp c (+ 1 o1 (- o2)))))
        (let ((integerizing-factor-as-terms (make-term-list (list (make-term 0 integerizing-factor)))))
          (let ((n-integerized (mul-terms integerizing-factor-as-terms n))
                (d-integerized (mul-terms integerizing-factor-as-terms d)))
            (let ((n-divided-by-gcd (car (div-terms n-integerized g)))
                  (d-divided-by-gcd (car (div-terms d-integerized g))))
              (let ((reduction-factor-n (foldr gcd
                                                (coeff (first-term n-divided-by-gcd))
                                                (map coeff n-divided-by-gcd))))
                (let ((reduction-factor-n-d (foldr gcd
                                                   reduction-factor-n
                                                   (map coeff d-divided-by-gcd))))
                  (let ((reduction-factor-as-terms (make-term-list (list (make-term 0 reduction-factor-n-d)))))
                    (let ((n-reduced (car (div-terms n-divided-by-gcd reduction-factor-as-terms)))
                          (d-reduced (car (div-terms d-divided-by-gcd reduction-factor-as-terms))))
                      (list n-reduced d-reduced))))))))))))

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))

  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))

  ;; representation of terms and term lists

  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (let ((reduced-terms (reduce-terms (term-list p1) (term-list p2))))
          (let ((nn (make-poly (variable p1) (car reduced-terms)))
                (dd (make-poly (variable p1) (cadr reduced-terms))))
                (list nn dd)))
        (error "Polys not in same var: REDUCE-POLY" (list p1 p2))))
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (make-poly 
         (variable p1)
         (add-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: 
              ADD-POLY"
               (list p1 p2))))

  (define (=zero-poly? p)
    (define (=zero-term-list? tl)
      (if (empty-termlist? tl)
          true
          (and (=zero? (coeff (first-term tl))) (=zero-term-list? (rest-terms tl)))))
    (=zero-term-list? (term-list p)))
   
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (make-poly 
         (variable p1)
         (mul-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: MUL-POLY"
               (list p1 p2))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (let ((q-and-r (div-terms (term-list p1)
                                  (term-list p2))))
          (list (make-poly (variable p1) (car q-and-r))
                (make-poly (variable p1) (cadr q-and-r))))
        (error "Polys not in same var: DIV-POLY"
               (list p1 p2))))
  
  (define (neg-poly p)
    (make-poly (variable p) (neg-term-list (term-list p))))

  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (make-poly 
         (variable p1)
         (gcd-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: GCD-POLY"
               (list p1 p2))))
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) 
         (map tag (div-poly p1 p2))))
  (put 'reduce '(polynomial polynomial)
        (lambda (x y) (map tag (reduce-poly x y))))
  (put 'make 'polynomial
       (lambda (var terms) 
         (tag (make-poly var terms))))

  (put '=zero? '(polynomial)
       =zero-poly?)

  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (add-poly p1 (neg-poly p2)))))

  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (gcd-poly p1 p2))))
  
  'done)

(install-polynomial-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define p1 
  (make-polynomial 'x '((1 1) (0 1))))
(define p2 
  (make-polynomial 'x '((3 1) (0 -1))))
(define p3 
  (make-polynomial 'x '((1 1))))
(define p4 
  (make-polynomial 'x '((2 1) (0 -1))))
(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))
(add rf1 rf2)
