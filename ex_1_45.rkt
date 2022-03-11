#lang sicp

;Exercise 1.45: We saw in 1.3.3 that attempting to compute square roots by naively finding a fixed point of y ↦ x / y does not converge, and that this can be fixed by average damping.
;The same method works for finding cube roots as fixed points of the average-damped y ↦ x / y 2 . Unfortunately, the process does not work for fourth roots—a single average damp is not
;enough to make a fixed-point search for y ↦ x / y 3 converge. On the other hand, if we average damp twice (i.e., use the average damp of the average damp of y ↦ x / y 3 ) the
;fixed-point search does converge. Do some experiments to determine how many average damps are required to compute n th roots as a fixed-point search based upon repeated average damping
;of y ↦ x / y n - 1 . Use this to implement a simple procedure for computing n th roots using fixed-point, average-damp, and the repeated procedure of Exercise 1.43. Assume that any ;arithmetic operations you need are available as primitives.

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (cond
    ((= n 1) f)
    ((= n 2) (compose f f))
    (else (compose f (repeated f (dec n))))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) 
    (average x (f x))))

(define (fourth-root x)
  (fixed-point 
   ((repeated average-damp 2)
    (lambda (y) 
      (/ x (* y y y))))
   1.0))

(define (fifth-root x)
  (fixed-point 
   ((repeated average-damp 2)
    (lambda (y) 
      (/ x (* y y y y))))
   1.0))

(define (sixth-root x)
  (fixed-point 
   ((repeated average-damp 2)
    (lambda (y) 
      (/ x (* y y y y y))))
   1.0))

(define (seventh-root x)
  (fixed-point 
   ((repeated average-damp 2)
    (lambda (y) 
      (/ x (* y y y y y y))))
   1.0))

(define (eighth-root x)
  (fixed-point 
   ((repeated average-damp 3)
    (lambda (y) 
      (/ x (* y y y y y y y))))
   1.0))

(define (sixteenth-root x)
  (fixed-point 
   ((repeated average-damp 4)
    (lambda (y) 
      (/ x (* y y y y y y y y y y y y y y y))))
   1.0))

;(define (pow b e) ;raise b to the e
;  (if (= e 1)
;      b
;      (* b (pow b (dec e)))))

(define (pow b e)
  (define (pow-iter acc e)
    (if (= e 0)
        acc
        (pow-iter (* acc b) (dec e))))
  (pow-iter 1 e))
  
(define (log2 x)
  (/ (log x) (log 2)))

(define (nth-root x n)
  (fixed-point 
   ((repeated average-damp ((compose floor log2) n)) ;log2 times average damping required
    (lambda (y) 
      (/ x (pow y (dec n)))))
   1.0))

(nth-root 20 18)
