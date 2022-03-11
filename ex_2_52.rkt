#lang racket

;Exercise 2.52: Make changes to the square limit of wave shown in Figure 2.9 by working at each of the levels described above. In particular:
;
;    Add some segments to the primitive wave painter of Exercise 2.49 (to add a smile, for example).
;    Change the pattern constructed by corner-split (for example, by using only one copy of the up-split and right-split images instead of two).
;    Modify the version of square-limit that uses square-of-four so as to assemble the corners in a different pattern. (For example, you might make the big Mr. Rogers look outward from
;each corner of the square.)

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
(define pt-seq-1-bis (list (make-vect 0.45 0.20) (make-vect 0.50 0.24) (make-vect 0.55 0.20)))
(define pt-seq-2 (list (make-vect 0.01 0.36) (make-vect 0.16 0.53) (make-vect 0.36 0.44) (make-vect 0.36 0.6) (make-vect 0.26 0.98)))
(define pt-seq-3 (list (make-vect 0.56 0) (make-vect 0.63 0.15) (make-vect 0.56 0.29) (make-vect 0.70 0.32) (make-vect 0.99 0.60)))
(define pt-seq-4 (list (make-vect 0.99 0.71) (make-vect 0.63 0.44) (make-vect 0.63 0.64) (make-vect 0.78 0.98)))
(define pt-seq-5 (list (make-vect 0.65 0.98) (make-vect 0.51 0.69) (make-vect 0.51 0.69) (make-vect 0.38 0.98)))
(define pt-seq (list pt-seq-1 pt-seq-1-bis pt-seq-2 pt-seq-3 pt-seq-4 pt-seq-5))

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

(define (transform-painter 
         painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                  (sub-vect (m corner1) 
                            new-origin)
                  (sub-vect (m corner2)
                            new-origin)))))))

(define (identity painter)
  painter)

(define (flip-hor painter)
  (transform-painter 
   painter
   (make-vect 1.0 0.0)   ; new origin
   (make-vect 0.0 0.0)   ; new end of edge1
   (make-vect 1.0 1.0))) ; new end of edge2

(define (flip-vert painter)
  (transform-painter 
   painter
   (make-vect 0.0 1.0)   ; new origin
   (make-vect 1.0 1.0)   ; new end of edge1
   (make-vect 0.0 0.0))) ; new end of edge2

(define (rot-180 painter)
  (transform-painter 
   painter
   (make-vect 1.0 1.0)   ; new origin
   (make-vect 0.0 1.0)   ; new end of edge1
   (make-vect 1.0 0.0))) ; new end of edge2

(define (rot-270 painter)
  (transform-painter 
   painter
   (make-vect 1.0 0.0)   ; new origin
   (make-vect 1.0 1.0)   ; new end of edge1
   (make-vect 0.0 0.0))) ; new end of edge2

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-up (transform-painter 
                       painter2
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 0.0)
                       split-point))
          (paint-down (transform-painter
                     painter1
                     split-point
                     (make-vect 1.0 0.5)
                     (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-up frame)
        (paint-down frame)))))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left  (transform-painter 
                        painter1
                        (make-vect 0.0 0.0)
                        split-point
                        (make-vect 0.0 1.0)))
          (paint-right (transform-painter
                        painter2
                        split-point
                        (make-vect 1.0 0.0)
                        (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (rot-90 painter)
  (rot-270 (rot-180 painter)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter 
                                  (- n 1))))
        (beside painter 
                (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter 
                               (- n 1))))
        (below painter 
                (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter 
                                (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split painter 
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right 
                         corner))))))

(define (square-limit painter n)
 (let ((combine4 
         (square-of-four flip-hor
                         identity
                         rot-180
                         flip-vert)))
    (combine4 (corner-split (flip-hor painter) n))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) 
                       (tr painter)))
          (bottom (beside (bl painter) 
                          (br painter))))
      (below bottom top))))

(define s (square-of-four wave->painter wave->painter wave->painter wave->painter))
;((corner-split wave->painter 4) (make-frame (make-vect 50 50) (make-vect 0 400) (make-vect 400 0)))
((square-limit wave->painter 4) (make-frame (make-vect 50 50) (make-vect 0 400) (make-vect 400 0)))
