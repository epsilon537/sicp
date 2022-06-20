#lang racket

;Exercise 3.57: How many additions are performed when we compute the n th Fibonacci number using the definition of fibs based on the add-streams procedure? Show that the number
;of additions would be exponentially greater if we had implemented (delay ⟨exp⟩) simply as (lambda () ⟨exp⟩), without using the optimization provided by the
;memo-proc procedure described in 3.5.1.192

(define-syntax delay
  (syntax-rules ()
    ((delay exp)
     (lambda () exp))))

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

(define (plus a b)
  (display "+")
  (+ a b))

(define (add-streams s1 s2) 
  (stream-map plus s1 s2))

(define fibs0 
  (cons-stream 
   0 (cons-stream
      1 (add-streams 
         (stream-cdr fibs0) fibs0))))

(define fibs1 
  (cons-stream 
   0 (cons-stream
      1 (add-streams 
         (stream-cdr fibs1) fibs2))))

(define fibs2 
  (cons-stream 
   0 (cons-stream
      1 (add-streams 
         (stream-cdr fibs2) fibs2))))

(define fibs3 
  (cons-stream 
   0 (cons-stream
      1 (add-streams 
         (stream-cdr fibs3) fibs3))))

(define fibs4 
  (cons-stream 
   0 (cons-stream
      1 (add-streams 
         (stream-cdr fibs4) fibs4))))

(display "stream-ref 0: ")
(stream-ref fibs0 0)
(newline)

(display "stream-ref 1: ")
(stream-ref fibs1 1)
(newline)

(display "stream-ref 2: ")
(stream-ref fibs2 2)
(newline)

(display "stream-ref 3: ")
(stream-ref fibs3 3)
(newline)

(display "stream-ref 4: ")
(stream-ref fibs4 4)
(newline)
