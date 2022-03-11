#lang sicp

;Exercise 2.12: Define a constructor make-center-percent that takes a center and a percentage tolerance and produces the desired interval. You must also define a selector percent that
;produces the percentage tolerance for a given interval. The center selector is the same as the one shown above.

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))

(define (width i)
  (/ (- (upper-bound i) 
        (lower-bound i)) 
     2))

(define (make-center-percent c p)
  (make-interval (* c (- 1 (/ p 100))) (* c (+ 1 (/ p 100)))))

(define (percent interval)
  (* 100 (/ (width interval) (center interval))))

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

(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "Divide by interval spanning 0: " y)
      (mul-interval x 
                    (make-interval 
                     (/ 1.0 (upper-bound y)) 
                     (/ 1.0 (lower-bound y))))))

(define four (make-interval 3.9 4.1))
(percent four)
(center four)
(make-center-percent 4.0 2.5)
