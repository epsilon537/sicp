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

;Exercise 3.82: Redo Exercise 3.5 on Monte Carlo integration in terms of streams. The stream version of estimate-integral will not have an argument telling how many trials to
;perform. Instead, it will produce a stream of estimates based on successively more trials.

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* range (random))))) ;Modified to used Racket's (random) which returns a decimal value in range (0,1).

(define (make-random-in-range-stream low high)
  (cons-stream
   (random-in-range low high)
   (make-random-in-range-stream low high)))

(define (monte-carlo experiment-stream 
                     passed 
                     failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) 
      passed 
      failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

;(define (estimate-integral P x1 x2 y1 y2)
;  (define (experiment-stream)
;    (cons-stream
;     (let ((x (random-in-range x1 x2))
;           (y (random-in-range y1 y2)))
;       (P x y))
;     (experiment-stream)))
;  (monte-carlo (experiment-stream) 0 0))

(define (estimate-integral P x1 x2 y1 y2)
  (monte-carlo (stream-map P (make-random-in-range-stream x1 x2) (make-random-in-range-stream x1 x2)) 0 0))

(define (unit-circle-test x y)
  (< (+ (* x x) (* y y)) 1))

(define pi-est (scale-stream (estimate-integral unit-circle-test -1 1 -1 1) 4.0))

(display-stream-n pi-est 1000)