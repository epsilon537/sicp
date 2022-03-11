#lang sicp

;Exercise 2.3: Implement a representation for rectangles in a plane. (Hint: You may want to make use of Exercise 2.2.) In terms of your constructors and selectors, create procedures that
;compute the perimeter and the area of a given rectangle. Now implement a different representation for rectangles. Can you design your system with suitable abstraction barriers, so that
;the same perimeter and area procedures will work using either representation?

;Math utils:
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

;Segments and points:

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

;Points utils
;Rotate clockwise around origin, angle in radians

(define (rotate-point p angle)
  (let ((x (x-point p))
        (y (y-point p)))
    (let ((x-acc (+ (* x (cos angle)) (* y (sin angle))))
          (y-acc (- (* y (cos angle)) (* x (sin angle)))))
      (make-point x-acc y-acc))))

(define (translate-point p1 p2)
  (make-point (+ (x-point p1) (x-point p2)) (+ (y-point p1) (y-point p2))))

;Segment utils
(define (seg-len seg)
  (let ((x1 (x-point (start-segment seg)))
        (x2 (x-point (end-segment seg)))
        (y1 (y-point (start-segment seg)))
        (y2 (y-point (end-segment seg))))
    (sqrt (+ (square (- x2 x1)) (square (- y2 y1))))))

;Rectangle Rep.1: Four points
;rotation = clockwise rotation in radians
(define (make-rect-pt upper-left-corner width height rotation)
  (define (translate-after-rotate x y)
    (let ((p-rot (rotate-point (make-point x y) rotation)))
        (translate-point p-rot upper-left-corner)))
  
  (let ((p0 upper-left-corner)
        (p1 (translate-after-rotate width 0))
        (p2 (translate-after-rotate width (- height)))
        (p3 (translate-after-rotate 0 (- height))))
    (cons (cons p0 p1) (cons p2 p3))))

;Retrieve get line segment from rectangle. Seg-ids 0-3 clockwise starting from upper-left corner (pre-rotation)
(define (get-seg-rect-pt rect seg-id)
  (let ((p0 (car (car rect)))
        (p1 (cdr (car rect)))
        (p2 (car (cdr rect)))
        (p3 (cdr (cdr rect))))
  (cond
    ((= seg-id 0) (make-segment p0 p1))
    ((= seg-id 1) (make-segment p1 p2))
    ((= seg-id 2) (make-segment p2 p3))
    (else (make-segment p3 p0)))))

;Rep.2: Four segments
;rotation = clockwise rotation in radians
(define (make-rect-seg upper-left-corner width height rotation)
  (define (translate-after-rotate x y)
    (let ((p-rot (rotate-point (make-point x y) rotation)))
        (translate-point p-rot upper-left-corner)))
  
  (let ((p0 upper-left-corner)
        (p1 (translate-after-rotate width 0))
        (p2 (translate-after-rotate width (- height)))
        (p3 (translate-after-rotate 0 (- height))))
    (cons (cons (make-segment p0 p1) (make-segment p1 p2)) (cons (make-segment p2 p3) (make-segment p3 p0)))))

;Retrieve get line segment from rectangle. Seg-ids 0-3 clockwise starting from upper-left corner (pre-rotation)
(define (get-seg-rect-seg rect seg-id)
  (let ((s0 (car (car rect)))
        (s1 (cdr (car rect)))
        (s2 (car (cdr rect)))
        (s3 (cdr (cdr rect))))
  (cond
    ((= seg-id 0) s0)
    ((= seg-id 1) s1)
    ((= seg-id 2) s2)
    (else s3))))

;Abstraction:
;Select the rep to use here:
(define make-rect make-rect-seg)
(define get-seg-rect get-seg-rect-seg)
;(define make-rect make-rect-pt)
;(define get-seg-rect get-seg-rect-pt)

(define (compute-perim rect)
  (let ((seg1 (get-seg-rect rect 0))
        (seg2 (get-seg-rect rect 1)))
    (* 2 (+ (seg-len seg1) (seg-len seg2)))))

(define (compute-area rect)
  (let ((seg1 (get-seg-rect rect 0))
        (seg2 (get-seg-rect rect 1)))
    (* (seg-len seg1) (seg-len seg2))))

