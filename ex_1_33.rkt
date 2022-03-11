#lang sicp

;Exercise 1.33: You can obtain an even more general version of accumulate (Exercise 1.32) by introducing the notion of a filter on the terms to be combined. That is, combine only those
;terms derived from values in the range that satisfy a specified condition. The resulting filtered-accumulate abstraction takes the same arguments as accumulate, together with an
;additional predicate of one argument that specifies the filter. Write filtered-accumulate as a procedure. Show how to express the following using filtered-accumulate:
;
;the sum of the squares of the prime numbers in the interval a to b (assuming that you have a prime? predicate already written)
;the product of all the positive integers less than n that are relatively prime to n (i.e., all positive integers i < n such that GCD ( i , n ) = 1 ). 

(define (filtered-accumulate pred? combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((pred? a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value)
  )

(define (square a)
  (* a a))

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (define (next n)
      (if (= n 2) 3 (+ n 2)))
    
    (cond ((> (square test-divisor) n) 
           n)
          ((divides? test-divisor n) 
           test-divisor)
          (else (find-divisor 
                 n 
                 (next test-divisor)))))
    
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum-of-squares-of-primes a b)
  (filtered-accumulate prime? + 0 square a inc b))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (rel-prime? a b)
  (= (gcd a b) 1))

(define (identity x) x)

(define (prod-of-rel-primes n)
  (define (rel-prime-to-n? a)
    (rel-prime? a n))
  
  (filtered-accumulate rel-prime-to-n? * 1 identity 1 inc (- n 1))
  )