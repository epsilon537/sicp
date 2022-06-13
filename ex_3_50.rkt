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

;Exercise 3.50: Complete the following definition, which generalizes stream-map to allow
;procedures that take multiple arguments, analogous to map in 2.2.1, Footnote 78.
;
;(define (stream-map proc . argstreams)
;  (if (⟨??⟩ (car argstreams))
;      the-empty-stream
;      (⟨??⟩
;       (apply proc (map ⟨??⟩ argstreams))
;       (apply stream-map
;              (cons proc 
;                    (map ⟨??⟩ 
;                         argstreams))))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc 
                    (map stream-cdr 
                         argstreams))))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define stream-1 (stream-enumerate-interval 100 300))
(define stream-2 (stream-enumerate-interval 400 600))
(define stream-3 (stream-enumerate-interval 700 900))
(define streams-mapped (stream-map + stream-1 stream-2 stream-3))
(display-stream streams-mapped)