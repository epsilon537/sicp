#lang sicp

;Exercise 2.41: Write a procedure to find all ordered triples of distinct positive integers i , j , and k less than or equal to a given integer n that sum to a given integer s .

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

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (make-triple-sum triple)
  (list (car triple) 
        (cadr triple)
        (caddr triple)
        (+ (car triple) (cadr triple) (caddr triple))))

(define (unique-triples n)
  (flatmap
   (lambda (i)
     (flatmap
         (lambda (j)
           (map (lambda (k) 
                  (list i j k))
                (enumerate-interval 
                 1 
                 (- j 1))))
         (enumerate-interval
          1
          (- i 1))))
   (enumerate-interval 1 n)))

(define (sums-to? t s)
  (= (+ (car t) (cadr t) (caddr t)) s))

(define (sums-to-triples n s)
  (define pred? (lambda (t) (sums-to? t s)))
  
  (map make-triple-sum
       (filter 
        pred?
        (unique-triples n))))