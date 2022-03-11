#lang sicp

;Exercise 1.39: A continued fraction representation of the tangent function was published in 1770 by the German mathematician J.H. Lambert:
;tan ⁡ x = x 1 - x 2 3 - x 2 5 - ... ,
;where x is in radians. Define a procedure (tan-cf x k) that computes an approximation to the tangent function based on Lambert’s formula. k specifies the number of terms to compute, as ;in Exercise 1.37.

(define (cont-frac-iter n d k)
  (define (cont-frac-loop i dacc)
    (if (= i 1)
        (/ (n i) dacc)
        (let ((q (/ (n i) dacc))
              (next-i (dec i)))
          (cont-frac-loop next-i (+ (d next-i) q)))))
  (cont-frac-loop k (d k)))

;N1=x, N2=-x^2, N3=-x^2
;D1=1, D2=3, D3=5
(define (tan-cf x k)
  (cont-frac-iter (lambda (i) (if (= i 1) x (- (* x x))))
                  (lambda (i) (+ (* 2 (- i 1)) 1))
                  k))