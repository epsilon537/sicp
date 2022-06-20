#lang racket

;Exercise 3.55: Define a procedure partial-sums that takes as argument a stream S and returns the stream whose elements are S 0 , S 0 + S 1 , S 0 + S 1 + S 2 , ... . For example, ;(partial-sums integers) should be the stream 1, 3, 6, 10, 15, ....

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

(define (integers-starting-from n)
  (cons-stream 
   n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc 
                    (map stream-cdr 
                         argstreams))))))

(define (add-streams s1 s2) 
  (stream-map + s1 s2))

(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (partial-sums s) (stream-cdr s))))

(define partial-sums-integers (partial-sums integers))

(stream-ref partial-sums-integers 0)
(stream-ref partial-sums-integers 1)
(stream-ref partial-sums-integers 2)
(stream-ref partial-sums-integers 3)
(stream-ref partial-sums-integers 4)
(stream-ref partial-sums-integers 5)
(stream-ref partial-sums-integers 6)
