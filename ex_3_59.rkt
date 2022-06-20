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

;Exercise 3.59: In 2.5.3 we saw how to implement a polynomial arithmetic system representing polynomials as lists of terms. In a similar way, we can work with power series, such
;as
;e x = 1 + x + 1 2 x 2 + 1 3 ⋅ 2 x 3 + 1 4 ⋅ 3 ⋅ 2 x 4 + ... , cos ⁡ x = 1 - 1 2 x 2 + 1 4 ⋅ 3 ⋅ 2 x 4 - ... , sin ⁡ x = x - 1 3 ⋅ 2 x 3 + 1 5 ⋅ 4 ⋅ 3 ⋅ 2 x 5 - ...
;represented as infinite streams. We will represent the series a 0 + a 1 x + a 2 x 2 + a 3 x 3 + ... as the stream whose elements are the coefficients a 0 , a 1 , a 2 , a 3 , ....
;
;    The integral of the series a 0 + a 1 x + a 2 x 2 + a 3 x 3 + ... is the series
;    c + a 0 x + 1 2 a 1 x 2 + 1 3 a 2 x 3 + 1 4 a 3 x 4 + ... ,
;    where c is any constant. Define a procedure integrate-series that takes as input a stream a 0 , a 1 , a 2 , ... representing a power series and returns the stream a 0 , 1 2
;a 1 , 1 3 a 2 , ... of coefficients of the non-constant terms of the integral of the series. (Since the result has no constant term, it doesn’t represent a power series; when we
;use integrate-series, we will cons on the appropriate constant.)
;    The function x ↦ e x is its own derivative. This implies that e x and the integral of e x are the same series, except for the constant term, which is e 0 = 1 . Accordingly,
;we can generate the series for e x as
;
;    (define exp-series
;      (cons-stream 
;       1 (integrate-series exp-series)))
;
;    Show how to generate the series for sine and cosine, starting from the facts that the derivative of sine is cosine and the derivative of cosine is the negative of sine:
;
;    (define cosine-series 
;      (cons-stream 1 ⟨??⟩))
;
;    (define sine-series
;      (cons-stream 0 ⟨??⟩))

;1.
(define ones (cons-stream 1 ones))

(define (integers-starting-from n)
  (cons-stream 
   n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (integrate-series s)
  (stream-map / s integers))

(define ones-integrated (integrate-series ones))
(stream-ref ones-integrated 0)
(stream-ref ones-integrated 1)
(stream-ref ones-integrated 2)
(stream-ref ones-integrated 3)
(stream-ref ones-integrated 4)
(stream-ref ones-integrated 5)

(newline)

;2.

(define exp-series
  (cons-stream 
   1 (integrate-series exp-series)))

(stream-ref exp-series 0)
(stream-ref exp-series 1)
(stream-ref exp-series 2)
(stream-ref exp-series 3)
(stream-ref exp-series 4)
(stream-ref exp-series 5)

(newline)

(define cosine-series 
  (cons-stream 1 (stream-map * minus-ones (integrate-series sine-series))))

(define minus-ones (cons-stream -1 minus-ones))

(define sine-series
  (cons-stream 0 (stream-map * ones (integrate-series cosine-series))))

(stream-ref sine-series 0)
(stream-ref sine-series 1)
(stream-ref sine-series 2)
(stream-ref sine-series 3)
(stream-ref sine-series 4)
(stream-ref sine-series 5)

(newline)

(stream-ref cosine-series 0)
(stream-ref cosine-series 1)
(stream-ref cosine-series 2)
(stream-ref cosine-series 3)
(stream-ref cosine-series 4)
(stream-ref cosine-series 5)