#lang sicp

;Exercise 2.29: A binary mobile consists of two branches, a left branch and a right branch. Each branch is a rod of a certain length, from which hangs either a weight or another binary
;mobile. We can represent a binary mobile using compound data by constructing it from two branches (for example, using list):
;
;(define (make-mobile left right)
;  (list left right))

;A branch is constructed from a length (which must be a number) together with a structure, which may be either a number (representing a simple weight) or another mobile:

;(define (make-branch length structure)
;  (list length structure))

;    Write the corresponding selectors left-branch and right-branch, which return the branches of a mobile, and branch-length and branch-structure, which return the components of a
;branch.
;    Using your selectors, define a procedure total-weight that returns the total weight of a mobile.
;    A mobile is said to be balanced if the torque applied by its top-left branch is equal to that applied by its top-right branch (that is, if the length of the left rod multiplied by
;the weight hanging from that rod is equal to the corresponding product for the right side) and if each of the submobiles hanging off its branches is balanced. Design a predicate that
;tests whether a binary mobile is balanced.
;    Suppose we change the representation of mobiles so that the constructors are
;
;    (define (make-mobile left right)
;      (cons left right))
;
;    (define (make-branch length structure)
;      (cons length structure))
;
;    How much do you need to change your programs to convert to the new representation?

(define (make-mobile-1 left right)
  (list left right))

(define (left-branch-1 m)
  (car m))

(define (right-branch-1 m)
  (cadr m))

(define (make-branch-1 length structure)
  (list length structure))

(define (branch-length-1 b)
  (car b))

(define (branch-structure-1 b)
  (cadr b))

(define (make-mobile-2 left right)
  (cons left right))

(define (left-branch-2 m)
  (car m))

(define (right-branch-2 m)
  (cdr m))

(define (make-branch-2 length structure)
  (cons length structure))

(define (branch-length-2 b)
  (car b))

(define (branch-structure-2 b)
  (cdr b))

;Switch here between implementation 1 and implementation 2
(define make-mobile make-mobile-2)
(define left-branch left-branch-2)
(define right-branch right-branch-2)
(define make-branch make-branch-2)
(define branch-length branch-length-2)
(define branch-structure branch-structure-2)

(define (total-weight s)
  (if (pair? s)
      (+ (total-weight (branch-structure (left-branch s))) (total-weight (branch-structure (right-branch s))))
      s))

(define (balanced? m)
  (if (pair? m)
      (let ((left-branch (left-branch m))
            (right-branch (right-branch m)))
        (let ((left-torque (* (branch-length left-branch) (total-weight (branch-structure left-branch))))
              (right-torque (* (branch-length right-branch) (total-weight (branch-structure right-branch)))))
          (and (= left-torque right-torque)
               (balanced? (branch-structure left-branch))
               (balanced? (branch-structure right-branch)))))
      true ;weights are balanced  
  ))

(define mobile-1 (make-mobile (make-branch 10 (make-mobile (make-branch 5 5) (make-branch 5 5))) (make-branch 20 10)))
(define mobile-2 (make-mobile (make-branch 10 (make-mobile (make-branch 5 10) (make-branch 5 10))) (make-branch 20 10)))

(total-weight mobile-1)
(balanced? mobile-1)
(total-weight mobile-2)
(balanced? mobile-2)