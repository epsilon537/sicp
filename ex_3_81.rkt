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

;Exercise 3.81: Exercise 3.6 discussed generalizing the random-number generator to allow one to reset the random-number sequence so as to produce repeatable sequences of
;“random”
;numbers. Produce a stream formulation of this same generator that operates on an input stream of requests to generate a new random number or to reset the sequence to a
;specified value and that produces the desired stream of random numbers. Don’t use assignment in your solution.

(define (random-update n)
  (let ((a 134775813 )
        (b 1)
        (m (expt 2 32)))
    (remainder (+ (* a n) b) m)))

(define (rand-stream init)
  (cons-stream init
               (stream-map random-update 
                           (rand-stream init))))
 
(define (make-rand-stream req-stream init-val)
  (define (rand-stream-req req-stream rnd-stream)
    (let ((op (stream-car req-stream))
          (rand-val (stream-car rnd-stream)))
      (cond ((eq? op 'generate)
             (cons-stream rand-val (rand-stream-req (stream-cdr req-stream) (stream-cdr rnd-stream))))
            ((eq? (stream-car req-stream) 'reset)
             (let ((new-init-val (stream-car (stream-cdr req-stream))))
               (if (not (integer? new-init-val))
                   (error "stream element following reset must be integer" new-init-val)
                   (cons-stream rand-val (rand-stream-req (stream-cdr (stream-cdr req-stream)) (rand-stream init-val))))))
            (else
             (error "unknown op " op)))))
  (rand-stream-req req-stream (rand-stream init-val)))

(define (args-to-stream . l)
  (if (empty? l)
      '()
      (cons-stream (car l) (apply args-to-stream (cdr l)))))

(define g-n-r (args-to-stream 'generate 'generate 'generate 'reset 1234 'generate 'generate 'generate 'reset))
(display-stream-n (make-rand-stream g-n-r 1234) 6)
