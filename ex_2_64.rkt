#lang sicp

;Exercise 2.64: The following procedure list->tree converts an ordered list to a balanced binary tree. The helper procedure partial-tree takes as arguments an integer n and list of at
;least n elements and constructs a balanced tree containing the first n elements of the list. The result returned by partial-tree is a pair (formed with cons) whose car is the
;constructed tree and whose cdr is the list of elements not included in the tree.

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))

;    Write a short paragraph explaining as clearly as you can how partial-tree works. Draw the tree produced by list->tree for the list (1 3 5 7 9 11).
;    What is the order of growth in the number of steps required by list->tree to convert a list of n elements?

;The ordered list is split into a left part, a midpoint, and a right part. Left part and right part are as much as possible equal in size.
;Left part and righ part are both turned into balanced trees the using partial-tree function recursively.
;The resulting left tree and right tree are then combined with the midpoint into a new node forming the top of the tree. Size left part and right part are
;as much as possible equal in size, the result left and right tree will be equal in size and the combined result will be maximally balanced.
;
;Wrong: The problem is repeatedly divided into two -> growth O(log(n))
;Right: The problem is repeatedly divided into two halves and each steps takes a fixed amount of time: T(n) -> 2*T(n/2) + O(1) -> O(n)