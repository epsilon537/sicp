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

(define (add-streams s1 s2) 
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

;Exercise 3.77: The integral procedure used above was analogous to the “implicit” definition of the infinite stream of integers in 3.5.2. Alternatively, we can give a
;definition of integral that is more like integers-starting-from (also in 3.5.2):
;
;(define (integral
;         integrand initial-value dt)
;  (cons-stream 
;   initial-value
;   (if (stream-null? integrand)
;       the-empty-stream
;       (integral 
;        (stream-cdr integrand)
;        (+ (* dt (stream-car integrand))
;           initial-value)
;        dt))))
;
;When used in systems with loops, this procedure has the same problem as does our original version of integral. Modify the procedure so that it expects the integrand as a ;delayed argument and hence can be used in the solve procedure shown above.

(define (integral
         delayed-integrand initial-value dt)
  (cons-stream 
   initial-value
   (let ((integrand (force delayed-integrand)))
     (if (stream-null? integrand)
         the-empty-stream
         (integral 
          (delay (stream-cdr integrand))
          (+ (* dt (stream-car integrand))
             initial-value)
          dt)))))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref 
 (solve (lambda (y) y) 1 0.001) 1000)
