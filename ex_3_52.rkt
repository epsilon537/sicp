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

;Exercise 3.52: Consider the sequence of expressions
;
;(define sum 0)
;
;(define (accum x)
;  (set! sum (+ x sum))
;  sum)
;
;(define seq 
;  (stream-map 
;   accum 
;   (stream-enumerate-interval 1 20)))
;
;(define y (stream-filter even? seq))
;
;(define z 
;  (stream-filter 
;   (lambda (x) 
;     (= (remainder x 5) 0)) seq))
;
;(stream-ref y 7)
;(display-stream z)
;
;What is the value of sum after each of the above expressions is evaluated? What is the printed response to evaluating the stream-ref and display-stream expressions? Would these
;responses differ if we had implemented (delay ⟨exp⟩) simply as (lambda () ⟨exp⟩) without using the optimization provided by memo-proc? Explain.

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

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

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

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream
       (proc (stream-car s))
       (stream-map proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq 
  (stream-map 
   accum 
   (stream-enumerate-interval 1 20)))

; seq:
; 0+1->1
; 1+2->3
; 3+3->6
; 6+4->10
; 10+5->15
; 15+6->21
; 21+7->28
; 28+8->36
; 36+9->45
; 45+10->55
; 55+11->66
; 66+12->78
; 78+13->91
; 91+14->105
; 105+15->120
; 120+16->136

(define y (stream-filter even? seq))

(define z 
  (stream-filter 
   (lambda (x) 
     (= (remainder x 5) 0)) seq))

(stream-ref y 7)
; 0+1->1
; 1+2->3 
; 3+3->6 y[0]
; 6+4->10 y[1]
; 10+5->15
; 15+6->21
; 21+7->28 y[2]
; 28+8->36 y[3]
; 36+9->45
; 45+10->55
; 55+11->66 y[4]
; 66+12->78 y[5]
; 78+13->91
; 91+14->105
; 105+15->120 y[6]
; 120+16->136 y[7] , which is also the value of sum at this point:
sum

(display-stream z)
; seq:
; 0+1->1
; 1+2->3
; 3+3->6
; 6+4->10  <-
; 10+5->15 <-
; 15+6->21
; 21+7->28
; 28+8->36
; 36+9->45 <-
; 45+10->55 <-
; 55+11->66
; 66+12->78
; 78+13->91
; 91+14->105 <-
; 105+15->120 <-
; 120+16->136
; 136+17->153
; 153+18->171
; 171+19->190 <-
; 190+20->210 <-
; sum is 210 at this point
sum

;Because the of memoization, accum is called just once for each element in the enumeration, so the sum behaves as expected.
;Without memoization accum gets called every time an element is referenced and each call as a side effect increases the sum -> sum increases more rapidly. See ex_3_52b.rkt.
