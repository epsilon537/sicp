#lang sicp

;Exercise 2.10: Ben Bitdiddle, an expert systems programmer, looks over Alyssa’s shoulder and comments that it is not clear what it means to divide by an interval that spans zero. Modify
;Alyssa’s code to check for this condition and to signal an error if it occurs.

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
;(div-interval four zero)
(div-interval minus-5 dot5)