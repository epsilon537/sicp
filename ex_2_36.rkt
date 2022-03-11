#lang sicp

;Exercise 2.36: The procedure accumulate-n is similar to accumulate except that it takes as its third argument a sequence of sequences, which are all assumed to have the same number of
;elements. It applies the designated accumulation procedure to combine all the first elements of the sequences, all the second elements of the sequences, and so on, and returns a
;sequence of the results. For instance, if s is a sequence containing four sequences, ((1 2 3) (4 5 6) (7 8 9) (10 11 12)), then the value of (accumulate-n + 0 s) should be the sequence
;(22 26 30). Fill in the missing expressions in the following definition of accumulate-n:
;
;(define (accumulate-n op init seqs)
;  (if (null? (car seqs))
;      nil
;      (cons (accumulate op init ⟨??⟩)
;            (accumulate-n op init ⟨??⟩))))
;
; Looking at the template, we're adding an element to the front of a list. In case of the example given, considering the first element of the result (22), that element is the
; accumulation of the first elements of all sub-lists. This is added onto (the reduction) the accumulate-n result applied to a list consisting of sublists with the first element
; removed.

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(define l (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 l)