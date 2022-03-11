#lang sicp

;Exercise 1.44: The idea of smoothing a function is an important concept in signal processing. If f is a function and d x is some small number, then the smoothed version of f is the
;function whose value at a point x is the average of f ( x - d x ) , f ( x ) , and f ( x + d x ) . Write a procedure smooth that takes as input a procedure that computes f and returns a
;procedure that computes the smoothed f . It is sometimes valuable to repeatedly smooth a function (that is, smooth the smoothed function, and so on) to obtain the n-fold smoothed
;function. Show how to generate the n-fold smoothed function of any given function using smooth and repeated from Exercise 1.43.

(define dx 0.00001)

(define (smooth f)
  (define (average a b c)
    (/ (+ a b c) 3))
  (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))

(define (stepf x)
  (if (> x 0) 1 0))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (cond
    ((= n 1) f)
    ((= n 2) (compose f f))
    (else (compose f (repeated f (dec n))))))

(define (smooth-n-fold f n)
  ((repeated smooth 3) f))

(display ((smooth stepf) (- dx)))
(newline)
(display ((smooth stepf) 0.0))
(newline)
(display ((smooth stepf) dx))
(newline)
(newline)
(display ((smooth (smooth (smooth stepf))) 0.0))
(newline)
(newline)
(display ((smooth-n-fold stepf 3) (- dx)))
(newline)
(display ((smooth-n-fold stepf 3) 0.0))
(newline)
(display ((smooth-n-fold stepf 3) dx))
(newline)