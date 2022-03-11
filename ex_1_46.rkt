#lang sicp

;Exercise 1.46: Several of the numerical methods described in this chapter are instances of an extremely general computational strategy known as iterative improvement. Iterative
;improvement says that, to compute something, we start with an initial guess for the answer, test if the guess is good enough, and otherwise improve the guess and continue the process
;using the improved guess as the new guess. Write a procedure iterative-improve that takes two procedures as arguments: a method for telling whether a guess is good enough and a method
;for improving a guess. Iterative-improve should return as its value a procedure that takes a guess as argument and keeps improving the guess until it is good enough. Rewrite the sqrt
;procedure of 1.1.7 and the fixed-point procedure of 1.3.3 in terms of iterative-improve.

(define (iterative-improve good-enough? improve-guess)
  (lambda (initial-guess)
    (define (iter guess) ;lambdas can still have lexically scope function definitions.
      (let ((next-guess (improve-guess guess)))
        (if (good-enough? guess next-guess) ;Assumption is that good-enough? requires two parameters: (current) guess and next guess.
            guess
            (iter next-guess))))
    (iter initial-guess))
  )

(define (average a b)
  (/ (+ a b) 2))

(define (square x)
  (* x x))

(define (sqrt x)
  (define (good-enough? guess next-guess) 
    (< (abs (- (square guess) x)) 0.001)) ;next guess is not used but iterative-improve requires it.
  
  (define (improve guess)
    (average guess (/ x guess)))

  (let ((sqrt-iter (iterative-improve good-enough? improve)))
    (sqrt-iter 1.0)))

(sqrt 100)

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  
  (define (improve guess)
    (f guess))

  (let ((try-iter (iterative-improve close-enough? improve)))
    (try-iter first-guess)))

(fixed-point cos 1.0)