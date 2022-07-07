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

;Exercise 3.68: Louis Reasoner thinks that building a stream of pairs from three parts is unnecessarily complicated. Instead of separating the pair ( S 0 , T 0 ) from the rest of ;the pairs in the first row, he proposes to work with the whole first row, as follows:
;
;(define (pairs s t)
;  (interleave
;   (stream-map
;    (lambda (x) 
;      (list (stream-car s) x))
;    t)
;   (pairs (stream-cdr s)
;          (stream-cdr t))))
;
;Does this work? Consider what happens if we evaluate (pairs integers integers) using Louis’s definition of pairs.

;A: the interleave call is pairs is no longer defered. Both arguments to interleave are evaluated before the call can be made. The 2nd argument is a call to pairs.
;-> We end up in an infinite loop.

