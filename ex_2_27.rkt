#lang sicp

;Exercise 2.27: Modify your reverse procedure of Exercise 2.18 to produce a deep-reverse procedure that takes a list as argument and returns as its value the list with its elements
;reversed and with all sublists deep-reversed as well. For example,
;
(define x 
  (list (list 1 2) (list 3 4)))
;
;x
;((1 2) (3 4))
;
;(reverse x)
;((3 4) (1 2))
;
;(deep-reverse x)
;((4 3) (2 1))

(define (reverse l)
  (if (null? l)
      nil
      (append (reverse (cdr l)) (list (car l)))))

;Take first elem of the list, reverse it and append it to the reverse rest of the list.
;Append takes two list elements, so to append an element to a list you need to do (append lst (list elem)).

(define (deep-reverse x)
  (if (pair? x) 
      (append (deep-reverse (cdr x)) (list (deep-reverse (car x))))
      x))

;I like this better, but that's not really the assignment
(define (deep-reverse-2 x)
  (if (pair? x)
      (map deep-reverse-2 (reverse x))
      x))

x
;((1 2) (3 4))

(reverse x)
;((3 4) (1 2))

(deep-reverse x)
;((4 3) (2 1))

(define l1 (list 1 3 (list 5 7) 9))
l1
(deep-reverse l1)

(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
l3
(deep-reverse l3)

(deep-reverse-2 x)

(deep-reverse-2 l1)

(deep-reverse-2 l3)