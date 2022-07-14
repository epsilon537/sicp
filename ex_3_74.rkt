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
     (memo-proc (lambda () exp)))))

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

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc 
                    (map stream-cdr 
                         argstreams))))))

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

(define (display-stream-n s n)
  (if (= n 0)
      "Done."
      (begin
        (display (stream-car s))
        (newline)
        (display-stream-n (stream-cdr s) (- n 1)))))

;Exercise 3.74: Alyssa P. Hacker is designing a system to process signals coming from physical sensors. One important feature she wishes to produce is a signal that describes the
;zero crossings of the input signal. That is, the resulting signal should be + 1 whenever the input signal changes from negative to positive, - 1 whenever the input signal
;changes from positive to negative, and 0 otherwise. (Assume that the sign of a 0 input is positive.) For example, a typical input signal with its associated zero-crossing signal
;would be
;
;... 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4 ...
;... 0 0  0  0  0   -1   0  0  0   0   1  0 0 ...
;
;In Alyssa’s system, the signal from the sensor is represented as a stream sense-data and the stream zero-crossings is the corresponding stream of zero crossings. Alyssa first
;writes a procedure sign-change-detector that takes two values as arguments and compares the signs of the values to produce an appropriate 0 , 1 , or - 1 . She then constructs
;her zero-crossing stream as follows:
;
;(define (make-zero-crossings
;         input-stream last-value)
;  (cons-stream
;   (sign-change-detector 
;    (stream-car input-stream) 
;    last-value)
;   (make-zero-crossings 
;    (stream-cdr input-stream)
;    (stream-car input-stream))))
;
;(define zero-crossings 
;  (make-zero-crossings sense-data 0))
;
;Alyssa’s boss, Eva Lu Ator, walks by and suggests that this program is approximately equivalent to the following one, which uses the generalized version of stream-map from ;Exercise 3.50:
;
;(define zero-crossings
;  (stream-map sign-change-detector 
;              sense-data 
;              ⟨expression⟩))
;
;Complete the program by supplying the indicated ⟨expression⟩.

(define (sign-change-detector a b)
  (if (< (* a b) 0)
      (if (> b 0) (- 1) 1)
      0))

(define (integers-starting-from n)
  (cons-stream 
   n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))
(define sin-stream (cons-stream 0 (stream-map sin integers)))
(define sense-data sin-stream)

(define zero-crossings
  (stream-map sign-change-detector 
              sense-data 
              (cons-stream 0 sense-data)))

(display-stream-n sense-data 100)
(display-stream-n zero-crossings 100)
