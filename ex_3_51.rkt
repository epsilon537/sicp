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

;Exercise 3.51: In order to take a closer look at delayed evaluation, we will use the following procedure, which simply returns its argument after ;printing it:
;
;(define (show x)
;  (display-line x)
;  x)
;
;What does the interpreter print in response to evaluating each expression in the following sequence?187
;
;(define x 
;  (stream-map 
;   show 
;   (stream-enumerate-interval 0 10)))
;
;(stream-ref x 5)
;(stream-ref x 7)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream
       (proc (stream-car s))
       (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))

(define (display-line x)
  (newline)
  (display x)
  (newline))

(define (show x)
  (display-line x)
  x)

(define x 
  (stream-map 
   show 
   (stream-enumerate-interval 0 10)))

(newline)
(display "stream-ref x 5:")
(newline)
(stream-ref x 5)
(newline)
(display "stream-ref x 7:")
(newline)
(stream-ref x 7)

