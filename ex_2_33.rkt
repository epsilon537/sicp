#lang sicp

;Exercise 2.33: Fill in the missing expressions to complete the following definitions of some basic list-manipulation operations as accumulations:
;
;(define (map p sequence)
;  (accumulate (lambda (x y) ??)
;              nil sequence))
;
;(define (append seq1 seq2)
;  (accumulate cons ?? ??))
;
;(define (length sequence)
;  (accumulate ?? 0 sequence))

(define (square x) (* x x))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) ;Note that we're consing the first element to the accumulation result of the rest of the sequence.
              nil sequence))                ;Follow that recursion to the beginning and you see that the first cons that actually evaluates is on the last element,
                                            ;then the one before gets added on etc., working back to the first element.

(map square (list 1 3 12 100))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2 3 4) (list 5 6 7 8))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence)) ;The x itself doesn't matter. Every time the accumulate op is called we add one to the accumulation value.

(length (list 1 2 3 4))
