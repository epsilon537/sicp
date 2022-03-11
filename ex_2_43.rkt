#lang sicp

;Exercise 2.43: Louis Reasoner is having a terrible time doing Exercise 2.42. His queens procedure seems to work, but it runs extremely slowly. (Louis never does manage to wait long
;enough for it to solve even the 6 × 6 case.) When Louis asks Eva Lu Ator for help, she points out that he has interchanged the order of the nested mappings in the flatmap, writing it as
;(flatmap
; (lambda (new-row)
;   (map (lambda (rest-of-queens)
;          (adjoin-position 
;           new-row k rest-of-queens))
;        (queen-cols (- k 1))))
; (enumerate-interval 1 board-size))

;Explain why this interchange makes the program run slowly. Estimate how long it will take Louis’s program to solve the eight-queens puzzle, assuming that the program in Exercise 2.42
;solves the puzzle in time T .
;

;Original:

;For each possible board of k-1 columns, add an element to the right, row-by-row.
;E.g. Board size k-1 1 / Pos (k, 1), Pos (k, 2), Board size k-1 2 / Pos (k, 1), Pos (k,2), Board size k-1 3 / Pos (k, 1), Pos (k, 2)
;All possible boards of size k-1 are generated just once.
;Recursion is on the outer loop here.

(flatmap
 (lambda (rest-of-queens)
   (map (lambda (new-row)
          (adjoin-position 
           new-row 
           k 
           rest-of-queens))
        (enumerate-interval 
         1 
         board-size)))
 (queen-cols (- k 1)))

;For each row, for each possible board of k-1 columns, add an element to the right in that row
;E.g. Pos (k, 1) / Board size k-1 1, 2, 3... Pos (k, 2) / Board size k-1 1, 2, 3...
;All possible boards of size k-1 are regenerated for each row in column k => All possible boards of size k-1 are regenerated board-size times.
;
;Recursion is taking place in the inner loop here. At every recursion level the number of queen-cols calls expands by a factor of k
;E.g. for a board size 4 we get:
;level 1: 4 calls
;level 2: 4*4 calls
;level 3: 4*4*4 calls
;level 4: 4*4*4*4 calls
;Original version only has 4 calls for same board size
;=> version below is (N^N)/N = N^(N-1) times more expensive than the original.

(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position 
           new-row k rest-of-queens))
        (queen-cols (- k 1))))
 (enumerate-interval 1 board-size))

