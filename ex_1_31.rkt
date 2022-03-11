#lang sicp

;Exercise 1.31:
;
;The sum procedure is only the simplest of a vast number of similar abstractions that can be captured as higher-order procedures.51 Write an analogous procedure called product that
;returns the product of the values of a function at points over a given range. Show how to define factorial in terms of product. Also use product to compute approximations to π using the
;formula52
;    π 4 = 2 ⋅ 4 ⋅ 4 ⋅ 6 ⋅ 6 ⋅ 8 ⋅ ⋯ 3 ⋅ 3 ⋅ 5 ⋅ 5 ⋅ 7 ⋅ 7 ⋅ ⋯ .
;    If your product procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive
;process.

(define (product-iter factor a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (factor a)))))
  (iter a 1))

(define (product-rec factor a next b)
  (if (> a b)
      1
      (* (factor a)
         (product-rec factor (next a) next b))))

(define (product factor a next b)
  (product-rec factor a next b)) ;Switch here between iterative and recursive variant.

(define (identity x) x)

(define (square a) (* a a))

(define (factorial n)
  (define (next a) (+ a 1))
  (product identity 1 next n))

;Rearrange the equation as pi = 4 * 2 * (4*4)/(3*3) * (6*6)/(5*5) ... / (n+1)
(define (pi n)
  (define (next a) (+ a 2))
  (define (factor a) (/ (square a) (square (- a 1))))
  (/ (* 8.0 (product factor 4 next (+ 4 n))) (+ 5 n)))