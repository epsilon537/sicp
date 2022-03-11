#lang sicp

;Exercise 2.48: A directed line segment in the plane can be represented as a pair of vectors—the vector running from the origin to the start-point of the segment, and the vector running
;from the origin to the end-point of the segment. Use your vector representation from Exercise 2.46 to define a representation for segments with a constructor make-segment and selectors
;start-segment and end-segment.

(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cadr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
    (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect scalar v)
    (make-vect (* (xcor-vect v) scalar) (* (ycor-vect v) scalar)))
  

(define (make-segment startv stopv)
  (list startv stopv))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cadr seg))
