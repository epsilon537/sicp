#lang racket

;Exercise 3.61: Let S be a power series (Exercise 3.59) whose constant term is 1. Suppose we want to find the power series 1 / S , that is, the series X such that S X = 1 . Write
;S = 1 + S R where S R is the part of S after the constant term. Then we can solve for X as follows:
;S ⋅ X = 1 , ( 1 + S R ) ⋅ X = 1 , X + S R ⋅ X = 1 , X = 1 - S R ⋅ X .
;In other words, X is the power series whose constant term is 1 and whose higher-order terms are given by the negative of S R times X . Use this idea to write a procedure
;invert-unit-series that computes 1 / S for a power series S with constant term 1. You will need to use mul-series from Exercise 3.60.

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

(define (add-streams s1 s2) 
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2)) (add-streams (scale-stream (stream-cdr s2) (stream-car s1)) (mul-series (stream-cdr s1) s2))))

(define ones (cons-stream 1 ones))
(define minus-ones (cons-stream -1 minus-ones))

(define (integers-starting-from n)
  (cons-stream 
   n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (integrate-series s)
  (stream-map / s integers))

(define (invert-unit-series s)
  (cons-stream 1 (scale-stream (mul-series (invert-unit-series s) (stream-cdr s)) -1)))

(define exp-series
  (cons-stream 
   1 (integrate-series exp-series)))

(define cosine-series 
  (cons-stream 1 (stream-map * minus-ones (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (stream-map * ones (integrate-series cosine-series))))

(define inv-exp-series (invert-unit-series exp-series))
(define exp*inv-exp-series (mul-series exp-series inv-exp-series))

(display "exp(x) series:")
(newline)
(stream-ref exp-series 0)
(stream-ref exp-series 1)
(stream-ref exp-series 2)
(stream-ref exp-series 3)
(stream-ref exp-series 4)
(stream-ref exp-series 5)

(display "1/exp(x) series:")
(newline)
(stream-ref inv-exp-series 0)
(stream-ref inv-exp-series 1)
(stream-ref inv-exp-series 2)
(stream-ref inv-exp-series 3)
(stream-ref inv-exp-series 4)
(stream-ref inv-exp-series 5)

(display "exp(x)*1/exp(x) series:")
(newline)
(stream-ref exp*inv-exp-series 0)
(stream-ref exp*inv-exp-series 1)
(stream-ref exp*inv-exp-series 2)
(stream-ref exp*inv-exp-series 3)
(stream-ref exp*inv-exp-series 4)
(stream-ref exp*inv-exp-series 5)

(define inv-cosine-series (invert-unit-series cosine-series))
(display "1/cos(x) series:")
(newline)
(stream-ref inv-cosine-series 0)
(stream-ref inv-cosine-series 1)
(stream-ref inv-cosine-series 2)
(stream-ref inv-cosine-series 3)
(stream-ref inv-cosine-series 4)
(stream-ref inv-cosine-series 5)
