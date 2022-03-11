#lang racket

;Exercise 2.49: Use segments->painter to define the following primitive painters:
;
;    The painter that draws the outline of the designated frame.
;    The painter that draws an “X” by connecting opposite corners of the frame.
;    The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
;    The wave painter.

(define nil '())

(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define (line v1 v2)
  ((draw-line vp) (make-posn (ycor-vect v1) (xcor-vect v1)) (make-posn (ycor-vect v2) (xcor-vect v2))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

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

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect 
      (scale-vect (xcor-vect v)
                  (edge1-frame frame))
      (scale-vect (ycor-vect v)
                  (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (line
        ((frame-coord-map frame) 
         (start-segment segment))
        ((frame-coord-map frame) 
         (end-segment segment))))
     segment-list)))

(define frame-outline->painter
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 1 0))
         (make-segment (make-vect 1 0) (make-vect 1 1))
         (make-segment (make-vect 1 1) (make-vect 0 1))
         (make-segment (make-vect 0 1) (make-vect 0 0)))))

(define frame-x->painter
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 1 1))
         (make-segment (make-vect 1 0) (make-vect 0 1)))))

(define frame-diamond->painter
  (segments->painter
   (list (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
         (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
         (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
         (make-segment (make-vect 0 0.5) (make-vect 0.5 0)))))

(define pt-seq-1 (list (make-vect 0.42 0.01) (make-vect 0.35 0.14) (make-vect 0.43 0.28) (make-vect 0.29 0.33) (make-vect 0.17 0.41) (make-vect 0 0.26)))
(define pt-seq-2 (list (make-vect 0.01 0.36) (make-vect 0.16 0.53) (make-vect 0.36 0.44) (make-vect 0.36 0.6) (make-vect 0.26 0.98)))
(define pt-seq-3 (list (make-vect 0.56 0) (make-vect 0.63 0.15) (make-vect 0.56 0.29) (make-vect 0.70 0.32) (make-vect 0.99 0.60)))
(define pt-seq-4 (list (make-vect 0.99 0.71) (make-vect 0.63 0.44) (make-vect 0.63 0.64) (make-vect 0.78 0.98)))
(define pt-seq-5 (list (make-vect 0.65 0.98) (make-vect 0.51 0.69) (make-vect 0.51 0.69) (make-vect 0.38 0.98)))
(define pt-seq (list pt-seq-1 pt-seq-2 pt-seq-3 pt-seq-4 pt-seq-5))

(define (pt-seq->seg-seq pt-seq)
  (define (points->seq points)
    (if (or (null? points) (null? (cdr points)))
        nil
        (let ((start (car points))
              (end (cadr points)))
          (cons (make-segment start end) (points->seq (cdr points))))))
  (flatmap points->seq pt-seq))

(define wave->painter
  (segments->painter (pt-seq->seg-seq pt-seq)))

;(frame-outline->painter (make-frame (make-vect 50 50) (make-vect 0 400) (make-vect 400 0)))
;(frame-x->painter (make-frame (make-vect 50 50) (make-vect 0 400) (make-vect 400 0)))
;(frame-diamond->painter (make-frame (make-vect 50 50) (make-vect 0 400) (make-vect 400 0)))
(wave->painter (make-frame (make-vect 50 50) (make-vect 0 400) (make-vect 400 0)))