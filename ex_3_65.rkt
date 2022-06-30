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

;Exercise 3.65: Use the series
;ln ⁡ 2 = 1 - 1 2 + 1 3 - 1 4 + ...
;to compute three sequences of approximations to the natural logarithm of 2, in the same way we did above for π . How rapidly do these sequences converge?

(define (add-streams s1 s2) 
  (stream-map + s1 s2))

(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (partial-sums s) (stream-cdr s))))

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define (ln-summands n)
  (cons-stream 
   (/ 1.0 n)
   (stream-map - (ln-summands (+ n 1)))))

(define ln-stream
  (scale-stream 
   (partial-sums (ln-summands 1)) 4))

(define (display-stream-limit s tolerance n)
  (let ((first (stream-ref s 0))
        (second (stream-ref s 1)))
    (let ((diff (- first second)))
      (if (and (> diff (- tolerance)) (< diff tolerance))
          second
          (begin
            (display n)
            (display " ")
            (display first)
            (display " ")
            (display diff)
            (newline)
            (display-stream-limit (stream-cdr s) tolerance (+ n 1)))))))

(display-stream-limit ln-stream 0.002 0)

(define (square x) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))     ; Sn-1
        (s1 (stream-ref s 1))     ; Sn
        (s2 (stream-ref s 2)))    ; Sn+1
    (cons-stream 
     (- s2 (/ (square (- s2 s1))
              (+ s0 (* -2 s1) s2)))
     (euler-transform (stream-cdr s)))))

(display-stream-limit 
 (euler-transform ln-stream) 0.002 0)

(define (make-tableau transform s)
  (cons-stream 
   s
   (make-tableau
    transform
    (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(display-stream-limit 
 (accelerated-sequence euler-transform
                       ln-stream) 0.002 0)