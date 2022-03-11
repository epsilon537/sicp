#lang sicp

;Exercise 2.66: Implement the lookup procedure for the case where the set of records is structured as a binary tree, ordered by the numerical values of the keys.

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (make-entry key other-data)
  (list key other-data))
(define (key entry)
  (car entry))

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

(define tree (list->tree (list
                          (make-entry 001 'banana)
                          (make-entry 002 'strawberry)
                          (make-entry 010 'pear)
                          (make-entry 050 'lemon)
                          (make-entry 100 'apple)
                          )))

(define (lookup given-key set-of-records)
    (if (null? set-of-records)
        false
        (let ((cur_entry (entry set-of-records)))
          (cond
            ((= given-key (key cur_entry)) cur_entry)
            ((< given-key (key cur_entry))
             (lookup
              given-key 
              (left-branch set-of-records)))
            (else
             (lookup
              given-key
              (right-branch set-of-records)))))))

