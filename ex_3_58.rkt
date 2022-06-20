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

;Exercise 3.58: Give an interpretation of the stream computed by the following procedure:
;
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) 
           den 
           radix)))
;
;(Quotient is a primitive that returns the integer quotient of two integers.) What are the successive elements produced by (expand 1 7 10)? What is produced by (expand 3 8 10)?
;
;This stream computes the decimals of num/den in the given radix
;E.g.
;(expand 1 7 10)
;1/7 in base 10 decimals:
;0.14285714285714285

(define expand-1-7-10 (expand 1 7 10))
(stream-ref expand-1-7-10 0)
(stream-ref expand-1-7-10 1)
(stream-ref expand-1-7-10 2)
(stream-ref expand-1-7-10 3)
(stream-ref expand-1-7-10 4)
(stream-ref expand-1-7-10 5)
;-> 1 4 2 8 5 7

(newline)

;3/8 in base 10 decimals:
;0.375
(define expand-3-8-10 (expand 3 8 10))
(stream-ref expand-3-8-10 0)
(stream-ref expand-3-8-10 1)
(stream-ref expand-3-8-10 2)
(stream-ref expand-3-8-10 3)
(stream-ref expand-3-8-10 4)
(stream-ref expand-3-8-10 5)
;-> 3 7 5 0 0 0
