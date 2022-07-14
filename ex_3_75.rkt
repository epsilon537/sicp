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

;Exercise 3.75: Unfortunately, Alyssa’s zero-crossing detector in Exercise 3.74 proves to be insufficient, because the noisy signal from the sensor leads to spurious zero
;crossings. Lem E. Tweakit, a hardware specialist, suggests that Alyssa smooth the signal to filter out the noise before extracting the zero crossings. Alyssa takes his advice
;and decides to extract the zero crossings from the signal constructed by averaging each value of the sense data with the previous value. She explains the problem to her
;assistant, Louis Reasoner, who attempts to implement the idea, altering Alyssa’s program as follows:
;
;(define (make-zero-crossings 
;         input-stream last-value)
;  (let ((avpt 
;         (/ (+ (stream-car input-stream) 
;               last-value) 
;            2)))
;    (cons-stream 
;     (sign-change-detector avpt last-value)
;     (make-zero-crossings 
;      (stream-cdr input-stream) avpt))))
;
;This does not correctly implement Alyssa’s plan. Find the bug that Louis has installed and fix it without changing the structure of the program. (Hint: You will need to
;increase the number of arguments to make-zero-crossings.)

(define (integers-starting-from n)
  (cons-stream 
   n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))
(define sin-stream (cons-stream 0 (stream-map sin integers)))
(define sense-data sin-stream)

(define (sign-change-detector a b)
  (if (< (* a b) 0)
      (if (> b 0) (- 1) 1)
      0))

(define (make-zero-crossings 
         input-stream last-value prev-avg)
  (let ((avpt 
         (/ (+ (stream-car input-stream) 
               last-value) 
            2)))
    (cons-stream 
     (sign-change-detector avpt prev-avg)
     (make-zero-crossings 
      (stream-cdr input-stream) (stream-car input-stream) avpt))))

(define zero-crossings 
  (make-zero-crossings sense-data 0 0))

(display-stream-n sense-data 100)
(display-stream-n zero-crossings 100)