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

;Exercise 3.60: With power series represented as streams of coefficients as in Exercise 3.59, adding series is implemented by add-streams. Complete the definition of the
;following procedure for multiplying series:
;
;(define (mul-series s1 s2)
;  (cons-stream ⟨??⟩ (add-streams ⟨??⟩ ⟨??⟩)))
;
;You can test your procedure by verifying that sin 2 ⁡ x + cos 2 ⁡ x = 1 , using the series from Exercise 3.59.

(define (add-streams s1 s2) 
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2)) (add-streams (scale-stream (stream-cdr s2) (stream-car s1)) (mul-series (stream-cdr s1) s2))))

(define ones (cons-stream 1 ones))

(define (integers-starting-from n)
  (cons-stream 
   n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (integrate-series s)
  (stream-map / s integers))

(define cosine-series 
  (cons-stream 1 (stream-map * minus-ones (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (stream-map * ones (integrate-series cosine-series))))

(define exp-series
  (cons-stream 
   1 (integrate-series exp-series)))

(define sinsqr+cossqr-series (add-streams (mul-series sine-series sine-series) (mul-series cosine-series cosine-series)))

(stream-ref sinsqr+cossqr-series 0)
(stream-ref sinsqr+cossqr-series 1)
(stream-ref sinsqr+cossqr-series 2)
(stream-ref sinsqr+cossqr-series 3)
(stream-ref sinsqr+cossqr-series 4)
(stream-ref sinsqr+cossqr-series 5)

