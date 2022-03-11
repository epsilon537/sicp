#lang sicp

;Exercise 2.37: Suppose we represent vectors v = ( v i ) as sequences of numbers, and matrices m = ( m i j ) as sequences of vectors (the rows of the matrix). For example, the matrix
;( 1 2 3 4 4 5 6 6 6 7 8 9 )
;is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this representation, we can use sequence operations to concisely express the basic matrix and vector operations.
;These operations (which are described in any book on matrix algebra) are the following:
;(dot-product v w) returns the sum Σ i v i w i ; (matrix-*-vector m v) returns the vector t , where t i = Σ j m i j v j ; (matrix-*-matrix m n) returns the matrix p , where p i j = Σ k m ;i k n k j ; (transpose m) returns the matrix n , where n i j = m j i .
;We can define the dot product as83
;
;(define (dot-product v w)
;  (accumulate + 0 (map * v w)))
;
;Fill in the missing expressions in the following procedures for computing the other matrix operations. (The procedure accumulate-n is defined in Exercise 2.36.)
;
;(define (matrix-*-vector m v)
;  (map ⟨??⟩ m));
;
;(define (transpose mat)
;  (accumulate-n ⟨??⟩ ⟨??⟩ mat))
;
;(define (matrix-*-matrix m n)
;  (let ((cols (transpose n)))
;    (map ⟨??⟩ m)))


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

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product (list 1 2 3) (list 4 5 6))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(matrix-*-vector (list (list 1 2 3) (list 3 4 5) (list 6 7 8)) (list 3 2 1))

(define (transpose mat)
  (accumulate-n cons nil mat))

(transpose  (list (list 1 2 3) (list 3 4 5) (list 6 7 8)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(matrix-*-matrix (list (list 1 2 3) (list 3 4 5) (list 6 7 8)) (list (list 1 2 3) (list 3 4 5) (list 6 7 8)))