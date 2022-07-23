#lang racket

;Exercise 3.56: A famous problem, first raised by R. Hamming, is to enumerate, in ascending order with no repetitions, all positive integers with no prime factors other than 2,
;3, or 5. One obvious way to do this is to simply test each integer in turn to see whether it has any factors other than 2, 3, and 5. But this is very inefficient, since, as the
;integers get larger, fewer and fewer of them fit the requirement. As an alternative, let us call the required stream of numbers S and notice the following facts about it.
;
;    S begins with 1.
;    The elements of (scale-stream S 2) are also elements of S.
;    The same is true for (scale-stream S 3) and (scale-stream S 5).
;    These are all the elements of S. 
;
;Now all we have to do is combine elements from these sources. For this we define a procedure merge that combines two ordered streams into one ordered result stream, eliminating ;repetitions:
;
;(define (merge s1 s2)
;  (cond ((stream-null? s1) s2)
;        ((stream-null? s2) s1)
;        (else
;         (let ((s1car (stream-car s1))
;               (s2car (stream-car s2)))
;           (cond ((< s1car s2car)
;                  (cons-stream 
;                   s1car 
;                   (merge (stream-cdr s1) 
;                          s2)))
;                 ((> s1car s2car)
;                  (cons-stream 
;                   s2car 
;                   (merge s1 
;                          (stream-cdr s2))))
;                 (else
;                  (cons-stream 
;                   s1car
;                   (merge 
;                    (stream-cdr s1)
;                    (stream-cdr s2)))))))))
;
;Then the required stream may be constructed with merge, as follows:
;
;(define S (cons-stream 1 (merge ⟨??⟩ ⟨??⟩)))
;
;Fill in the missing expressions in the places marked ⟨??⟩ above.

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

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream 
                   s1car 
                   (merge (stream-cdr s1) 
                          s2)))
                 ((> s1car s2car)
                  (cons-stream 
                   s2car 
                   (merge s1 
                          (stream-cdr s2))))
                 (else
                  (cons-stream 
                   s1car
                   (merge 
                    (stream-cdr s1)
                    (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))
(stream-ref S 0)
(stream-ref S 1)
(stream-ref S 2)
(stream-ref S 3)
(stream-ref S 4)
(stream-ref S 5)
(stream-ref S 6)
(stream-ref S 7)
(stream-ref S 8)
(stream-ref S 9)
(stream-ref S 10)
(stream-ref S 11)
(stream-ref S 12)
(stream-ref S 13)