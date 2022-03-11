#lang sicp

;Exercise 2.6: In case representing pairs as procedures wasn’t mind-boggling enough, consider that, in a language that can manipulate procedures, we can get by without numbers (at least
;insofar as nonnegative integers are concerned) by implementing 0 and the operation of adding 1 as
;
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
;
;This representation is known as Church numerals, after its inventor, Alonzo Church, the logician who invented the λ-calculus.
;
;Define one and two directly (not in terms of zero and add-1). (Hint: Use substitution to evaluate (add-1 zero)).
;Give a direct definition of the addition procedure + (not in terms of ;repeated application of add-1).

(define (tst x)
  (display x)
  (newline)
  (inc x))

;(add-1 zero)
;=>
;(add-1 (lambda (f) (lambda (x) x)))
;=>
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
;=>
;(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
;=>
(define one (lambda (f) (lambda (x) (f x))))

;(add-1 (lambda (f) (lambda (x) (f x))))
;=>
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
;=>
;(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
;=>
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
 (lambda (f) ;The result of the addition should be a number, i.e. a lambda (f)...
   (lambda (x) ;...returning a lambda (x)
     ((b f) ((a f) x))))) ;First we repeatedly apply (f x) a times (e.g. (f (f (f x))) if a=3), then we repeatedly apply (f x) b times to the result.
                          ;This is equivalent to applying (f x) (a+b) times, which is the expected representation of (a+b).