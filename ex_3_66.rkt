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

;Exercise 3.66: Examine the stream (pairs integers integers). Can you make any general comments about the order in which the pairs are placed into the stream? For example,
;approximately how many pairs precede the pair (1, 100)? the pair (99, 100)? the pair (100, 100)? (If you can make precise mathematical statements here, all the better. But feel
;free to give more qualitative answers if you find yourself getting bogged down.)

;If you consider the upper diagonal matrix in the text, every other element in the stream, i.e. 50% of the elements, come from the first row, the remaining elements come from the
;other rows (25% from 2nd row, 12.5% from 3rd,...)
;
;By the time we hit the pair (1, 100) (first row, 100the column), we've taken 100 elements from the first row and 100 elemnts from the the other rows -> 200 pairs.
;
;The upper diagonal matrix generating sequence looks like this:
;
;1	2	4	6	8	10	12	14	16	18	20	22
;	3	5	9	13	17	21	25	29	33	37	41
;		7	11	19	27	35	43	51	59	67	
;			15	23	39	55	71	...			
;				31	47	79	111	...			
;					63	...					
;
;The values on the diagonal can be computed as (2^(n+1))-1, where n is the row/column number starting from 0. The number of pairs preceding that point is one less
;-> (2^(n+1))-2
;The 2nd value on each row can be computed as (2^(n+1))-1+(2^n), where n is the row number starting from 0. The number of pairs preceding that point is one less
;-> (2^(n+1))+(2^n)-2
;
;The pair (99, 100) is the 2nd element of row 99 -> (2^(99+1))+(2^99)-2 = (2^100) + (2^99) - 2.
;
;The pair (100,100) is on the diagonal -> (2^(100+1))-2

