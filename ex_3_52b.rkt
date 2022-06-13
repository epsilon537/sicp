#lang racket

(define (memo-proc proc)
  (let ((already-run? false)
        (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define-syntax delay
  (syntax-rules ()
    ((delay exp)
     (lambda () exp))))

(define (force delayed-object)
  (delayed-object))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define stream-null? null?)
(define the-empty-stream '())
(define nil '())

;Exercise 3.52: Consider the sequence of expressions
;
;(define sum 0)
;
;(define (accum x)
;  (set! sum (+ x sum))
;  sum)
;
;(define seq 
;  (stream-map 
;   accum 
;   (stream-enumerate-interval 1 20)))
;
;(define y (stream-filter even? seq))
;
;(define z 
;  (stream-filter 
;   (lambda (x) 
;     (= (remainder x 5) 0)) seq))
;
;(stream-ref y 7)
;(display-stream z)
;
;What is the value of sum after each of the above expressions is evaluated? What is the printed response to evaluating the stream-ref and display-stream expressions? Would these
;responses differ if we had implemented (delay ⟨exp⟩) simply as (lambda () ⟨exp⟩) without using the optimization provided by memo-proc? Explain.

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) 
         the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream 
          (stream-car stream)
          (stream-filter 
           pred
           (stream-cdr stream))))
        (else (stream-filter 
               pred 
               (stream-cdr stream)))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream
       (proc (stream-car s))
       (stream-map proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  (display "x = ")
  (display x)
  (display " , sum = ")
  (display sum)
  (newline)
  sum)

(define seq 
  (stream-map 
   accum 
   (stream-enumerate-interval 1 20)))
;sum is now 1, the first element of seq has been computed
(newline)

(define y (stream-filter even? seq))
;sum is now 6, the first event element. seq has been computed until the third element (x=3).
(newline)

(define z 
  (stream-filter 
   (lambda (x) 
     (= (remainder x 5) 0)) seq))
(newline)
; the filter pulls the first element (x=1), which seq's stream-car, so this doesn't result in an accum call.
; the filter pulls the 2nd element (x=2), but sum is already 6, so sum becomes 8.
; From there we continue until sum becomes a multiple of 5, which happens to be 15, at x=4.

(stream-ref y 7)
(newline)
;y had already been computed until seq. element x=3. We continue from x=4 onward and a sum value of 15.
;We keep going until y[7], the 8th even value has been found. This happens at sum=162.

(display-stream z)
;We continue z here from x=5 and sum=162 onwards until we reach x=20.
