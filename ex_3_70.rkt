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

;Exercise 3.70: It would be nice to be able to generate streams in which the pairs appear in some useful order, rather than in the order that results from an ad hoc interleaving
;process. We can use a technique similar to the merge procedure of Exercise 3.56, if we define a way to say that one pair of integers is “less than” another. One way to do this
;is to define a “weighting function” W ( i , j ) and stipulate that ( i 1 , j 1 ) is less than ( i 2 , j 2 ) if W ( i 1 , j 1 ) < W ( i 2 , j 2 ) . Write a procedure
;merge-weighted that is like merge, except that merge-weighted takes an additional argument weight, which is a procedure that computes the weight of a pair, and is used to
;determine the order in which elements should appear in the resulting merged stream.197 Using this, generalize pairs to a procedure weighted-pairs that takes two streams,
;together with a procedure that computes a weighting function, and generates the stream of pairs, ordered according to weight. Use your procedure to generate
;
;    the stream of all pairs of positive integers ( i , j ) with i ≤ j ordered according to the sum i + j ,
;the stream of all pairs of positive integers ( i , j ) with i ≤ j , where neither i nor j is divisible by 2, 3, or 5, and the pairs are ordered according to the sum 2 i + 3 j +
;5 i j .

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
(define integers-doubled (stream-map (lambda (x) (* x 2)) integers))
(define pairs-1 (pairs integers integers))
(define pairs-2 (pairs integers-doubled integers-doubled))

(display-stream-n pairs-1 20)
(display-stream-n pairs-2 20)
(display-stream-n (merge-weighted pairs-1 pairs-2 (lambda (p) (+ (car p) (cadr p)))) 20)

(define sum-weighted-integers (pairs-weighted integers integers (lambda (p) (+ (car p) (cadr p)))))

(display-stream-n sum-weighted-integers 20)

(define integers-undivisible-by-2-3-5 (stream-filter (lambda (x) (and (not (= 0 (remainder x 2))) (not (= 0 (remainder x 3))) (not (= 0 (remainder x 5))))) integers))
(display-stream-n integers-undivisible-by-2-3-5 20)

(define (weight-fun p)
  (let ((i (car p))
        (j (cadr p)))
    (+ (* 2 i) (* 3 j) (* 5 i j))))

(define weight-fun-weighted-integers (pairs-weighted integers-undivisible-by-2-3-5 integers-undivisible-by-2-3-5 weight-fun))
(display-stream-n weight-fun-weighted-integers 20)