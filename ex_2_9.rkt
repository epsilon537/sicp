#lang sicp

;Exercise 2.9: The width of an interval is half of the difference between its upper and lower bounds. The width is a measure of the uncertainty of the number specified by the interval.
;For some arithmetic operations the width of the result of combining two intervals is a function only of the widths of the argument intervals, whereas for others the width of the
;combination is not a function of the widths of the argument intervals. Show that the width of the sum (or difference) of two intervals is a function only of the widths of the intervals
;being added (or subtracted). Give examples to show that this is not true for multiplication or division.

;a-interval: lower-bound = a - a-width, upper-bound = a + a-width
;b-interval: lower-bound = b - b-width, upper-bound = b + b-width
;
;lower-bound-sum: a-interval + b-interval = lower-bound a + lower-bound b = a - a-width + b - b-width
;upper-bound-sum: a-interval + b-interval = upper-bound a + upper-bound b = a + a-width + b + b-width
;width-sum = (upper-bound-sum - lower-bound-sum)/2: ((a + a-width + b + b-width) - (a - a-width + b - b-width))/2 = (2*a-width + 2*b-width)/2 = a-width + b-width
;=> not a function of a or b

(define (make-interval a b) (cons a b))

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
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y)))))

(define (width-interval interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))

(define four (make-interval 3.9 4.1))
(define ten (make-interval 9.9 10.1))
(width-interval (mul-interval four four))
;->0.7999999999999998
(width-interval (mul-interval ten ten))
;->1.999999999999993
;four and ten have same width, but four squared and ten squared have different width -> multiplication is not just a function of width.

(width-interval (div-interval four four))
;->0.05003126954346465
(width-interval (div-interval ten ten))
;->0.02000200020001991
;four and ten have same width, but four/four and ten/ten have different width -> division is not just a function of width.