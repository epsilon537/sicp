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

;Exercise 3.72: In a similar way to Exercise 3.71 generate a stream of all numbers that can be written as the sum of two squares in three different ways (showing how they can be ;so written).

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (weight s1car) (weight s2car))
                  (cons-stream 
                   s1car 
                   (merge-weighted (stream-cdr s1) 
                                   s2 weight)))
                 ((> (weight s1car) (weight s2car))
                  (cons-stream 
                   s2car 
                   (merge-weighted s1 
                                   (stream-cdr s2)
                                   weight)))
                 (else
                  (if (and (= (car s1car) (car s2car)))
                     (cons-stream 
                      s1car
                      (merge-weighted 
                       (stream-cdr s1)
                       (stream-cdr s2)
                       weight)) 
                     (cons-stream
                      s1car
                      (cons-stream 
                       s2car
                       (merge-weighted 
                        (stream-cdr s1)
                        (stream-cdr s2)
                        weight))))))))))

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

(define (pairs-weighted s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs-weighted (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define (integers-starting-from n)
  (cons-stream 
   n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (sum-of-squares p)
  (let ((i (car p))
        (j (cadr p)))
    (+ (* i i) (* j j))))

(define squared-pairs-stream (pairs-weighted integers integers sum-of-squares))
(display-stream-n squared-pairs-stream 100)

(define (find-sums-of-squares s)
  (if (and (= (sum-of-squares (stream-ref s 0)) (sum-of-squares (stream-ref s 1))) (= (sum-of-squares (stream-ref s 0)) (sum-of-squares (stream-ref s 2))))
      (cons-stream (list (sum-of-squares (stream-ref s 0)) (stream-ref s 0) (stream-ref s 1) (stream-ref s 2))
                   (find-sums-of-squares (stream-cdr s)))
      (find-sums-of-squares (stream-cdr s))))

(display-stream-n (find-sums-of-squares squared-pairs-stream) 5)