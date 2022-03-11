#lang sicp

;Exercise 2.11: In passing, Ben also cryptically comments: “By testing the signs of the endpoints of the intervals, it is possible to break mul-interval into nine cases, only one of
;which requires more than two multiplications.” Rewrite this procedure using Ben’s suggestion.

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) 
                    (upper-bound y)) ;y reverses sign so upper and lower bound switch places
                 (- (upper-bound x) 
                    (lower-bound y))))  ;y reverses sign so upper and lower bound switch places

(define (mul-interval x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond
      ((and (< lx 0) (< ux 0))
       (cond
         ((and (< ly 0) (< uy 0))
          (make-interval (* ux uy) (* lx ly)))
         ((and (< ly 0) (>= uy 0))
          (make-interval (* lx uy) (* lx ly)))
         (else
          (make-interval (* lx uy) (* ux ly)))))
      ((and (< lx 0) (>= ux 0))
       (cond
         ((and (< ly 0) (< uy 0))
          (make-interval (* ux ly) (* lx ly)))
         ((and (< ly 0) (>= uy 0))
          (make-interval
           (min (* lx uy) (* ux ly))
           (max (* lx ly) (* ux uy))
           ))
         (else
          (make-interval (* ly uy) (* ux uy)))))
      (else
       (cond
         ((and (< ly 0) (< uy 0))
          (make-interval (* ux ly) (* lx uy)))
         ((and (< ly 0) (>= uy 0))
          (make-interval (* ux ly) (* ux uy)))
         (else
          (make-interval (* lx ly) (* ux uy))))
       ))))

(define (mul-interval-orig x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "Divide by interval spanning 0: " y)
      (mul-interval x 
                    (make-interval 
                     (/ 1.0 (upper-bound y)) 
                     (/ 1.0 (lower-bound y))))))

(define four (make-interval 3.9 4.1))
(define minus-5 (make-interval -5.2 -4.8))
(define zero (make-interval -0.2 0.2))
(define dot5 (make-interval 0 1.0))
(mul-interval four zero)
(mul-interval minus-5 dot5)
(mul-interval-orig four zero)
(mul-interval-orig minus-5 dot5)
;(div-interval four zero)
;(div-interval minus-5 dot5)