#lang sicp

;Exercise 2.69: The following procedure takes as its argument a list of symbol-frequency pairs (where no symbol appears in more than one pair) and generates a Huffman encoding tree
;according to the Huffman algorithm.
;
;(define (generate-huffman-tree pairs)
;  (successive-merge 
;   (make-leaf-set pairs)))
;
;Make-leaf-set is the procedure given above that transforms the list of pairs into an ordered set of leaves. Successive-merge is the procedure you must write, using make-code-tree to
;successively merge the smallest-weight elements of the set until there is only one element left, which is the desired Huffman tree. (This procedure is slightly tricky, but not really
;complicated. If you find yourself designing a complex procedure, then you are almost certainly doing something wrong. You can take significant advantage of the fact that we are using an
;ordered set representation.)

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define pairs (list (list 'A 8) (list 'B 3) (list 'C 1) (list 'D 1) (list 'E 1) (list 'F 1) (list 'G 1) (list 'H 1)))

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

(define (successive-merge node-set)
  (if (= 1 (length node-set))
      (car node-set)
      (let ((new-node (make-code-tree (car node-set) (cadr node-set))))
        (successive-merge (adjoin-set new-node (cddr node-set))))))

(define tree (generate-huffman-tree pairs))

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

(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch 
                (car bits) 
                current-branch)))
          (if (leaf? next-branch)
              (cons 
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: 
               CHOOSE-BRANCH" bit))))
