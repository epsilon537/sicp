#lang racket

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

(define (timed-prime-test n)
  (newline)
  (display n)
  (newline)
  (start-prime-test n))

(define (start-prime-test n)
  (if (prime? n) (time (prime? n) (display n) (newline)) #f))

(define (search-for-primes n numToSearch)
  (cond ((= numToSearch 0)
         (display "Done"))
        ((start-prime-test n)
         (search-for-primes (+ n 1) (- numToSearch 1)))
        (else
         (search-for-primes (+ n 1) numToSearch))))

;New:
;> (search-for-primes 10000000000000 3)
;10000000000037
;cpu time: 141 real time: 146 gc time: 0 --- 188 in orig ==> ratio: 1.29
;10000000000051
;cpu time: 141 real time: 140 gc time: 0 --- 188 in orig ==> ratio: 1.33
;10000000000099
;cpu time: 172 real time: 169 gc time: 0 --- 203 in orig ==> ratio: 1.18
;Done
;> (search-for-primes 100000000000000 3)
;100000000000031
;cpu time: 453 real time: 454 gc time: 0 --- 593 in orig ==> ratio: 1.31
;100000000000067
;cpu time: 437 real time: 436 gc time: 0 --- 610 in orig ==> ratio: 1.40
;100000000000097
;cpu time: 438 real time: 435 gc time: 15 --- 593 in orig ==> ratio: 1.35 
;Done
;> (search-for-primes 1000000000000000 3)
;1000000000000037
;cpu time: 1375 real time: 1378 gc time: 32 --- 1922 in orig ==> ratio: 1.40
;1000000000000091
;cpu time: 1312 real time: 1308 gc time: 0 --- 1859 in orig ==> ratio: 1.42
;1000000000000159
;cpu time: 1296 real time: 1301 gc time: 0 --- 1875 in orig ==> ratio: 1.45
;Done
;>
;Less steps, but each step is more expensive due to overhead of next function and conditional in next function.
