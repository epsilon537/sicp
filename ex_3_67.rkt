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

;Exercise 3.67: Modify the pairs procedure so that (pairs integers integers) will produce the stream of all pairs of integers ( i , j ) (without the condition i ≤ j ). Hint: You ;will need to mix in an additional stream.

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))


(define (interleave-3 s1 s2 s3)
  (interleave s1 (interleave s2 s3)))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave-3
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (stream-map (lambda (x) 
                  (list x (stream-car t)))
                (stream-cdr s))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (integers-starting-from n)
  (cons-stream 
   n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define int-pairs (pairs integers integers))

(define (display-stream-n s n)
  (if (= n 0)
      "Done."
      (begin
        (display (stream-car s))
        (newline)
        (display-stream-n (stream-cdr s) (- n 1)))))

(display-stream-n int-pairs 100)