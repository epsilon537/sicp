#lang sicp

;Exercise 2.70: The following eight-symbol alphabet with associated relative frequencies was designed to efficiently encode the lyrics of 1950s rock songs. (Note that the “symbols” of an
;“alphabet” need not be individual letters.)
;
;
;A    2    NA  16
;BOOM 1    SHA  3
;GET  2    YIP  9
;JOB  2    WAH  1
;
;Use generate-huffman-tree (Exercise 2.69) to generate a corresponding Huffman tree, and use encode (Exercise 2.68) to encode the following message:
;
;Get a job
;Sha na na na na na na na na
;
;Get a job
;Sha na na na na na na na na
;
;Wah yip yip yip yip 
;yip yip yip yip yip
;Sha boom
;
;How many bits are required for the encoding? What is the smallest number of bits that would be needed to encode this song if we used a fixed-length code for the eight-symbol alphabet?

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

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

(define (successive-merge node-set)
  (if (= 1 (length node-set))
      (car node-set)
      (let ((new-node (make-code-tree (car node-set) (cadr node-set))))
        (successive-merge (adjoin-set new-node (cddr node-set))))))

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

;A    2    NA  16
;BOOM 1    SHA  3
;GET  2    YIP  9
;JOB  2    WAH  1
(define pairs (list (list 'A 2) (list 'NA 16) (list 'BOOM 1) (list 'SHA 3) (list 'GET 2) (list 'YIP 9) (list 'JOB 2) (list 'WAH 1)))
(define tree (generate-huffman-tree pairs))

;Get a job
;Sha na na na na na na na na
;
;Get a job
;Sha na na na na na na na na
;
;Wah yip yip yip yip 
;yip yip yip yip yip
;Sha boom
(define message '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))
(length message)
(define encoded-message (encode message tree))
(length encoded-message)
(decode (encode message tree) tree)
  
;Fixed length code: 8 symbols -> 3 bits per symbol. 36 symbols -> 3*36 = 108 bits

