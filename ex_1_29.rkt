#lang sicp

;Exercise 1.29: Simpson’s Rule is a more accurate method of numerical integration than the method illustrated above. Using Simpson’s Rule, the integral of a function f between a and b is
;approximated as
;h 3 ( y 0 + 4 y 1 + 2 y 2 + 4 y 3 + 2 y 4 + ⋯ + 2 y n - 2 + 4 y n - 1 + y n ) ,
;where h = ( b - a ) / n , for some even integer n , and y k = f ( a + k h ) . (Increasing n increases the accuracy of the approximation.) Define a procedure that takes as arguments f ,
;a , b , and n and returns the value of the integral, computed using Simpson’s Rule. Use your procedure to integrate cube between 0 and 1 (with n = 100 and n = 1000 ), and compare the
;results to those of the integral procedure shown above.

(define (cube a) (* a a a))

(define (sum-with-k term a next k)
  (if (= -1 k)
      0
      (+ (term a k)
         (sum-with-k term (next a) next (- k 1)))))

(define (simpson f a b n)  
  (define (simpson-with-h h)
    (define (term x k)
      (cond ((= k 0) (f x))
            ((= k n) (f x))
            ((odd? k) (* 4 (f x)))
            ((even? k) (* 2 (f x)))))

    (define (next a) (+ a h))
    
    (* (/ h 3) (sum-with-k term a next n))
    )

  (simpson-with-h (/ (- b a) n))
  )
