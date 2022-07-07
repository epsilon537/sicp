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

;Exercise 3.69: Write a procedure triples that takes three infinite streams, S , T , and U , and produces the stream of triples ( S i , T j , U k ) such that i ≤ j ≤ k . Use
;triples to generate the stream of all Pythagorean triples of positive integers, i.e., the triples ( i , j , k ) such that i ≤ j and i 2 + j 2 = k 2 .

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
  (cons-stream
   (cons (stream-car u) (stream-car (pairs s t)))
   (interleave
    (stream-map (lambda (x) (cons (stream-car u) x)) (stream-cdr (pairs s t)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))
  
(define (integers-starting-from n)
  (cons-stream 
   n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (display-stream-n s n)
  (if (= n 0)
      "Done."
      (begin
        (display (stream-car s))
        (newline)
        (display-stream-n (stream-cdr s) (- n 1)))))

(define (square x) (* x x))
(define square-stream (stream-map square integers))

(define pythagorean-triples
  (stream-filter (lambda (l) (= (+ (list-ref l 0) (list-ref l 1)) (list-ref l 2))) (triples square-stream square-stream square-stream)))

(display-stream-n (triples integers integers integers) 100)
(display-stream-n (triples square-stream square-stream square-stream) 100)
(display-stream-n pythagorean-triples 5)
