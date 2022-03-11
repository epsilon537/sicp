#lang sicp

;Exercise 2.72: Consider the encoding procedure that you designed in Exercise 2.68. What is the order of growth in the number of steps needed to encode a symbol? Be sure to include the
;number of steps needed to search the symbol list at each node encountered. To answer this question in general is difficult. Consider the special case where the relative frequencies of
;the n symbols are as described in Exercise 2.71, and give the order of growth (as a function of n ) of the number of steps needed to encode the most frequent and least frequent symbols
;in the alphabet.

(define (contains? lst element)
  (if (null? lst)
      false
      (if (equal? (car lst) element)
          true
          (contains? (cdr lst) element))))

(define (encode-symbol symbol tree)
  (if (contains? (symbols tree) symbol)
      (if (leaf? tree)
          '()
          (let ((left (left-branch tree))
                (right (right-branch tree)))
            (cond
              ((contains? (symbols left) symbol) (cons 0 (encode-symbol symbol left)))
              ((contains? (symbols right) symbol) (cons 1 (encode-symbol symbol right)))
              (else (error "Symbol not in left or right branch" (symbols left) (symbols right) symbol)))))                
      (error "Node does not contain symbol: " (symbols tree) symbol)))

Each pass through encode-symbol in this implementation invokes 'contains?' 2 or 3 times. 'contains' is O(n)
For the least frequent symbol (n-1) nodes are searched: (n-1)*O(n) = O(n^2). The probability of this happening is 1/(2**n-1).
For the most frequen symbol 1 node is searched: 1*O(n) = O(n). The chances of this happening is 0.5.

-> encoded-symbol has O(n) to O(n^2) growth, leaning more to O(n)
