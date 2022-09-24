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

;Exercise 4.13: Scheme allows us to create new bindings for variables by means of define, but provides no way to get rid of bindings. Implement for the evaluator a special form
;make-unbound! that removes the binding of a given symbol from the environment in which the make-unbound! expression is evaluated. This problem is not completely specified. For
;example, should we remove only the binding in the first frame of the environment? Complete the specification and justify any choices you make.

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

(define (frame-find-binding frame var remove)
  (cond ((null? frame)
         '())
        ((empty? (car frame))
         (frame-find-binding (cdr frame) var remove))
        ((eq? var (car (car frame)))
         (if remove
             (set-car! frame '())
             (car frame)))
        (else (frame-find-binding (cdr frame) var remove))))

(define (get-binding-var binding)
  (car binding))

(define (get-binding-val binding)
  (cdr binding))

(define (set-binding-val! binding val)
  (set-cdr! binding val))

(define (frame-contains-variable? frame var)
  (pair? (frame-find-binding frame var #f)))
  
(define (frame-lookup-variable frame var)
  (let ((binding (frame-find-binding frame var #f)))
    (if (not (pair? binding))
        (error "frame-lookup-variable: frame does not contain binding for given variable.")
        (get-binding-val binding))))
      
(define (frame-modify-binding! frame var val)
  (let ((binding (frame-find-binding frame var #f)))
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

(define (make-unbound! var env) 
  (let ((frame (first-frame env)))
    (if (not (frame-contains-variable? frame var))
        (error "Unbound variable: make-unbound!" var)
        (frame-find-binding frame var #t))))

(define env (extend-environment (list 'y 'z) (list 10 20) the-empty-environment))
(define ext-env (extend-environment (list 't 'u) (list 100 200) env))
(lookup-variable-value 't ext-env)
(display "unbinding...")
(newline)
(make-unbound! 't ext-env)
;(lookup-variable-value 't ext-env)
(display "rebinding...")
(newline)
(define-variable! 't 1000 ext-env)
(lookup-variable-value 't ext-env)

;A: We are only unbinding the variable in the first frame. Unbinding in enclosing environments can give confusing results where enclosing functions may unexpectedly see
;some of their variables removed, even though they didn't invoke make-unbound themselves).