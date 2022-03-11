#lang sicp

;Exercise 1.27: Demonstrate that the Carmichael numbers listed in Footnote 47 really do fool the Fermat test. That is, write a procedure that takes an integer n and tests whether a n is
;congruent to a modulo n for every a < n , and try your procedure on the given Carmichael numbers.

(define (square a)
  (* a a))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

;This fermat-test scans through all a's from 2 to (n-1)
(define (fermat-test n)
  (define (fermat-test-iter a)
    (cond ((= a 1) true)
          ((= (expmod a n n) a) (fermat-test-iter (- a 1)))
          (else false)))
  (fermat-test-iter (- n 1)))

(fermat-test 561)
(fermat-test 1105)
(fermat-test 1729)
(fermat-test 2465)
(fermat-test 2821)
(fermat-test 6601)
