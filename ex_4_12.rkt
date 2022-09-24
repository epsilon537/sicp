#lang racket

(require rnrs/mutable-pairs-6) ;for set-car! and set-cdr!
(require compatibility/mlist) ;for mlist

(define cons mcons)
(define car mcar)
(define cdr mcdr)
(define list mlist)
(define length mlength)
(define map mmap)
(define pair? mpair?)

;Exercise 4.12: The procedures define-variable!, set-variable-value! and lookup-variable-value can be expressed in terms of more abstract procedures for traversing the
;environment structure. Define abstractions that capture the common patterns and redefine the three procedures in terms of these abstractions.

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cond ((not (= (length variables) (length values)))
         (error "make-frame: variables and values must be of equal length"))
        ((empty? variables) '())
        (else         
         (cons (cons (car variables)
                     (car values))
               (make-frame (cdr variables) (cdr values))))))

(define (frame-variables frame)
  (map car frame))

(define (frame-values frame)
  (map cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))

(define (frame-find-binding frame var)
  (cond ((null? frame)
         '())
        ((eq? var (car (car frame)))
         (car frame))
        (else (frame-find-binding (cdr frame) var))))

(define (get-binding-var binding)
  (car binding))

(define (get-binding-val binding)
  (cdr binding))

(define (set-binding-val! binding val)
  (set-cdr! binding val))

(define (frame-contains-variable? frame var)
  (pair? (frame-find-binding frame var)))
  
(define (frame-lookup-variable frame var)
  (let ((binding (frame-find-binding frame var)))
    (if (not (pair? binding))
        (error "frame-lookup-variable: frame does not contain binding for given variable.")
        (get-binding-val binding))))
      
(define (frame-modify-binding! frame var val)
  (let ((binding (frame-find-binding frame var)))
    (if (not (pair? binding))
        (error "frame-modify-binding: frame does not contain binding for given variable.")
        (set-binding-val! binding val))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" 
                 vars 
                 vals)
          (error "Too few arguments supplied" 
                 vars 
                 vals))))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (if (frame-contains-variable? frame var)
        (frame-modify-binding! frame var val)
        (add-binding-to-frame! var val frame))))

(define (set-variable-value! var val env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable: SET!" var)
      (let ((frame (first-frame env)))
        (if (not (frame-contains-variable? frame var))
            (set-variable-value! var val (enclosing-environment env))
            (frame-modify-binding! frame var val)))))

(define (lookup-variable-value var env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable: " var)
      (let ((frame (first-frame env)))
        (if (not (frame-contains-variable? frame var))
            (lookup-variable-value var (enclosing-environment env))
            (frame-lookup-variable frame var)))))

;Orig:
;(define (define-variable! var val env)
;  (let ((frame (first-frame env)))
;    (define (scan vars vals)
;      (cond ((null? vars)
;             (add-binding-to-frame! 
;              var val frame))
;            ((eq? var (car vars))
;             (set-car! vals val))
;            (else (scan (cdr vars) 
;                        (cdr vals)))))
;    (scan (frame-variables frame)
;          (frame-values frame))))
;
;(define (set-variable-value! var val env)
;  (define (env-loop env)
;    (define (scan vars vals)
;      (cond ((null? vars)
;             (env-loop 
;              (enclosing-environment env)))
;            ((eq? var (car vars))
;             (set-car! vals val))
;            (else (scan (cdr vars) 
;                        (cdr vals)))))
;    (if (eq? env the-empty-environment)
;        (error "Unbound variable: SET!" var)
;        (let ((frame (first-frame env)))
;          (scan (frame-variables frame)
;                (frame-values frame)))))
;  (env-loop env))
;
;(define (lookup-variable-value var env)
;  (define (env-loop env)
;    (define (scan vars vals)
;      (cond ((null? vars)
;             (env-loop 
;              (enclosing-environment env)))
;            ((eq? var (car vars))
;             (car vals))
;            (else (scan (cdr vars) 
;                        (cdr vals)))))
;    (if (eq? env the-empty-environment)
;        (error "Unbound variable" var)
;        (let ((frame (first-frame env)))
;          (scan (frame-variables frame)
;                (frame-values frame)))))
;  (env-loop env))

(define env (extend-environment (list 'y 'z) (list 10 20) the-empty-environment))
(define ext-env (extend-environment (list 't 'u) (list 100 200) env))
(define-variable! 'x 1 ext-env)
(display "defined x: ")
(display (lookup-variable-value 'x ext-env))
(newline)

(define-variable! 'x 2 ext-env)
(display "redefined x: ")
(display (lookup-variable-value 'x ext-env))
(newline)

(set-variable-value! 'x 3 ext-env)
(display "set x: ")
(display (lookup-variable-value 'x ext-env))
(newline)

(display "lookup t: ")
(display (lookup-variable-value 't ext-env))
(newline)
(set-variable-value! 't 300 ext-env)
(display "modified t: ")
(display (lookup-variable-value 't ext-env))
(newline)

(display "lookup z: ")
(display (lookup-variable-value 'z ext-env))
(newline)
(set-variable-value! 'z 30 ext-env)
(display "modified z: ")
(display (lookup-variable-value 'z ext-env))
(newline)