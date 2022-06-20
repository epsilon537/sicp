#lang racket

;Exercise 3.62: Use the results of Exercise 3.60 and Exercise 3.61 to define a procedure div-series that divides two power series. Div-series should work for any two series, ;provided that the denominator series begins with a nonzero constant term. (If the denominator has a zero constant term, then div-series should signal an error.) Show how to use ;div-series together with the result of Exercise 3.59 to generate the power series for tangent.

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

(define (div-series n d)
  (let ((const-term (stream-car d)))
    (newline)
    (display "const-term = ")
    (display const-term)
    (newline)
    (if (= 0 const-term)
        (error "0 constant term in denominator")
        (let ((d-normalized (scale-stream d (/ 1 const-term))))
          (scale-stream (mul-series n (invert-unit-series d-normalized)) (/ 1 const-term))))))

(define inv-cosine-series (invert-unit-series cosine-series))

(define tan-series (div-series sine-series cosine-series))

(display "sine series:")
(newline)
(stream-ref sine-series 0)
(stream-ref sine-series 1)
(stream-ref sine-series 2)
(stream-ref sine-series 3)
(stream-ref sine-series 4)
(stream-ref sine-series 5)

(newline)
(display "cosine series:")
(newline)

(stream-ref cosine-series 0)
(stream-ref cosine-series 1)
(stream-ref cosine-series 2)
(stream-ref cosine-series 3)
(stream-ref cosine-series 4)
(stream-ref cosine-series 5)

(newline)
(display "inv(cosine) series:")
(newline)

(stream-ref inv-cosine-series 0)
(stream-ref inv-cosine-series 1)
(stream-ref inv-cosine-series 2)
(stream-ref inv-cosine-series 3)
(stream-ref inv-cosine-series 4)
(stream-ref inv-cosine-series 5)

(newline)
(display "tangent series:")
(newline)

(stream-ref tan-series 0)
(stream-ref tan-series 1)
(stream-ref tan-series 2)
(stream-ref tan-series 3)
(stream-ref tan-series 4)
(stream-ref tan-series 5)

(newline)


