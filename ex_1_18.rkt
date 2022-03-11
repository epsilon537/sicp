#lang sicp

;Exercise 1.18: Using the results of Exercise 1.16 and Exercise 1.17, devise a procedure that generates an iterative process for multiplying two integers in terms of adding, doubling,
;and halving and uses a logarithmic number of steps.40

(define (double a)
  (* a 2))

(define (half a)
  (/ a 2))

(define (fast-mul a b)
  (cond ((= b 0) 
         0)
        ((even? b) 
         (double (fast-mul a (half b))))
        (else 
         (+ a (fast-mul a (- b 1))))))

(define (fast-mul-iter a b)
  (define (iter s a b)
    (cond ((= b 0)
           s)
          ((even? b)
           (iter s (double a) (half b)))
          (else
           (iter (+ s a) a (- b 1)))))
  (iter 0 a b)
  )