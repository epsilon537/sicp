#lang sicp

;Exercise 1.38: In 1737, the Swiss mathematician Leonhard Euler published a memoir De Fractionibus Continuis, which included a continued fraction expansion for e - 2 , where e is the
;base of the natural logarithms. In this fraction, the N i are all 1, and the D i are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, .... Write a program that uses your cont-frac
;procedure from Exercise 1.37 to approximate e , based on Euler’s expansion. 

;For the iterative implementation, start with the k-th term (i=k) and walk back to the first term (i=1). 
(define (cont-frac-iter n d k)
  (define (cont-frac-loop i dacc)
    (if (= i 1)
        (/ (n i) dacc)
        (let ((q (/ (n i) dacc))
              (next-i (dec i)))
          (cont-frac-loop next-i (+ (d next-i) q)))))
  (cont-frac-loop k (d k)))

;The convention used in cont-frac-iter above is that the first term has i=1, not 0. Keep this in mind when computing Di below.
(define (D i)
  (if (= (remainder (- i 2) 3) 0)
      (/ (* 2 (+ i 1)) 3)
      1))

(cont-frac-iter (lambda (i) 1.0)
                D
                12) ; 12 iterations required for four accurate decimal places.