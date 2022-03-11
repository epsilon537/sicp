#lang sicp

;Exercise 2.14: Demonstrate that Lem is right. Investigate the behavior of the system on a variety of arithmetic expressions. Make some intervals A and B , and use them in computing the
;expressions A / A and A / B . You will get the most insight by using intervals whose width is a small percentage of the center value. Examine the results of the computation in
;center-percent form (see Exercise 2.12).

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

(define (par1 r1 r2)
  (div-interval 
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval 
     one
     (add-interval 
      (div-interval one r1) 
      (div-interval one r2)))))

(define one-hundred (make-center-percent 100 1))
(define one-thousand (make-center-percent 1000 2))

(display "1000/1000 percent: ")
(percent (div-interval one-thousand one-thousand))
(newline)
(display "1000/100 percent: ")
(percent (div-interval one-thousand one-hundred))
(newline)
(display "par1 100 1000 (R1R2)/(R1+R2): ")
(center (par1 one-hundred one-thousand))
(percent (par1 one-hundred one-thousand))
(newline)
(display "par2 100 1000 1/((1/R1)+(1/R2)): ")
(center (par2 one-hundred one-thousand))
(percent (par2 one-hundred one-thousand))
(newline)

;1000/1000 percent: 3.9984006397441028
;
;1000/100 percent: 2.9994001199760136
;
;par1 100 1000 (R1R1)/(R1+R2): 91.01250951793175
;4.90568197302085
;
;par2 100 1000 1/((1/R1)+(1/R2)): 90.90833932036387
;1.090933893542143

;Observation 1: percentage of division is sum of percentages of num and denom.
;Observation 2: par2 is more accurate than par1.
;
;Due to Obs1: percentage of interval = percentage of 1/interval:
;> (percent (div-interval (make-interval 1 1) one-thousand))
;2.0000000000000053
;> (percent (div-interval (make-interval 1 1) one-hundred))
;1.0000000000000036
;
;Dus to ex_2_9: width of sum = sum of widths => the percentage of a sum of a small and a large quantity is mostly determined by the percentage of the largest quantity
;E.g. percent(100-interval + 1000-interval) ~= percent(1000-interval)  (assuming 100 and 1000 intervals have fairly small percentages).
;In par2 implementation 1/100 is the largest quantity => percentage of (1/100 + 1/1000) ~= percentage of 1/100
;The 1/x operation does not affect the percentage, so the percentage of the par2 implementation is mostly determined by the percentage of the one hundred interval.
;
;In the par1 implementation the percentage of the result ~= percentage of one-hundred + percentage of one-thousand + percentage of one-thousand (from denom)
