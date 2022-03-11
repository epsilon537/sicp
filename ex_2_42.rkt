#lang sicp

;Exercise 2.42: The “eight-queens puzzle” asks how to place eight queens on a chessboard so that no queen is in check from any other (i.e., no two queens are in the same row, column, or
;diagonal). One possible solution is shown in Figure 2.8. One way to solve the puzzle is to work across the board, placing a queen in each column. Once we have placed k - 1 queens, we
;must place the k th queen in a position where it does not check any of the queens already on the board. We can formulate this approach recursively: Assume that we have already generated
;the sequence of all possible ways to place k - 1 queens in the first k - 1 columns of the board. For each of these ways, generate an extended set of positions by placing a queen in each
;row of the k th column. Now filter these, keeping only the positions for which the queen in the k th column is safe with respect to the other queens. This produces the sequence of all
;ways to place k queens in the first k columns. By continuing this process, we will produce not only one solution, but all solutions to the puzzle.
;SVG
;    Figure 2.8: A solution to the eight-queens puzzle.

;We implement this solution as a procedure queens, which returns a sequence of all solutions to the problem of placing n queens on an n × n chessboard. Queens has an internal procedure
;queen-cols that returns the sequence of all ways to place queens in the first k columns of the board.

;(define (queens board-size)
;  (define (queen-cols k)
;    (if (= k 0)
;        (list empty-board)
;        (filter
;         (lambda (positions) 
;           (safe? k positions))
;         (flatmap
;          (lambda (rest-of-queens)
;            (map (lambda (new-row)
;                   (adjoin-position 
;                    new-row 
;                    k 
;                    rest-of-queens))
;                 (enumerate-interval 
;                  1 
;                  board-size)))
;          (queen-cols (- k 1))))))
;  (queen-cols board-size))


;In this procedure rest-of-queens is a way to place k - 1 queens in the first k - 1 columns, and new-row is a proposed row in which to place the queen for the k th column. Complete the
;program by implementing the representation for sets of board positions, including the procedure adjoin-position, which adjoins a new row-column position to a set of positions, and
;empty-board, which represents an empty set of positions. You must also write the procedure safe?, which determines for a set of positions, whether the queen in the k th column is safe
;with respect to the others. (Note that we need only check whether the new queen is safe—the other queens are already guaranteed safe with respect to each other.) 


;Representation of set of board positions: (list (list col row) (list col row) ...)
;E.g. for example given:
(define ex-board-pos (list (list 1 3) (list 2 7) (list 3 2) (list 4 8) (list 5 5) (list 6 1) (list 7 4) (list 8 6)))

(define (empty? seq)
  (eq? nil seq))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
                       (cdr sequence)))))

(define (contains? e s)
  (not (empty? (filter (lambda (x) (equal? e x)) s))))
           
(define empty-board nil)

;Given a set of board positions, add a column with queen in row k 
(define (adjoin-position new-row k set-of-board-positions)
  (cons (list k new-row) set-of-board-positions))

(define (safe? k positions)
  ;Find row in column k
  (let ((r (cadr (car (filter (lambda (pair) (= (car pair) k)) positions)))))

    ;Generate conflicting positions
    (define conflict-hor (map (lambda (i) (list i r)) (enumerate-interval 1 (- k 1))))

    (define (conflict-diag-iter kk rr r-op)
        (if (< kk 1)
            nil
            (cons (list kk rr) (conflict-diag-iter (- kk 1) (r-op rr 1) r-op))))
    
    (define (conflict-diag-up k r)
      (conflict-diag-iter (- k 1) (- r 1) -))

    (define (conflict-diag-down k r)
      (conflict-diag-iter (- k 1) (+ r 1) +))

    (empty? (filter
             (lambda (x) (contains? x positions))
             (append conflict-hor (conflict-diag-down k r) (conflict-diag-up k r))))
  ))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;Returns a sequence of sets of board positions, i.e. a list of lists.
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
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
          (queen-cols (- k 1))))))
  (queen-cols board-size))
