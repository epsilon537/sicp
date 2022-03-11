#lang sicp

;Exercise 2.1: Define a better version of make-rat that handles both positive and negative arguments. Make-rat should normalize the sign so that if the rational number is positive, both
;the numerator and denominator are positive, and if the rational number is negative, only the numerator is negative.

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;Returns 1 or -1 depending on x being >= or < than 0.
(define (sign x)
  (if (>= x 0) 1 -1))

(define (make-rat n d)    
  (let ((s (sign (* n d)))
        (g (gcd n d)))
    (let ((num-norm (* s (abs (/ n g))))
          (denom-norm (abs (/ d g))))
    (cons num-norm denom-norm))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (make-rat 4 6))
(print-rat (make-rat -4 6))
(print-rat (make-rat 4 -6))
(print-rat (make-rat -4 -6))


