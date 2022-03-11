#lang racket

;Exercise 1.22: Most Lisp implementations include a primitive called runtime that returns an integer that specifies the amount of time the system has been running (measured, for example,
;in microseconds). The following timed-prime-test procedure, when called with an integer n , prints n and checks to see if n is prime. If n is prime, the procedure prints three asterisks
;followed by the amount of time used in performing the test.
;
;(define (timed-prime-test n)
;  (newline)
;  (display n)
;  (start-prime-test n (runtime)))
;
;(define (start-prime-test n start-time)
;  (if (prime? n)
;      (report-prime (- (runtime) 
;                       start-time))))
;
;(define (report-prime elapsed-time)
;  (display " *** ")
;  (display elapsed-time))
;
;Using this procedure, write a procedure search-for-primes that checks the primality of consecutive odd integers in a specified range. Use your procedure to find the three smallest
;primes larger than 1000; larger than 10,000; larger than 100,000; larger than 1,000,000. Note the time needed to test each prime. Since the testing algorithm has order of growth of Θ (
;n ) , you should expect that testing for primes around 10,000 should take about 10 times as long as testing for primes around 1000. Do your timing data bear this out? How well do the
;data for 100,000 and 1,000,000 support the Θ ( n ) prediction? Is your result compatible with the notion that programs on your machine run in time proportional to the number of steps
;required for the computation?

(define (square a)
  (* a a))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

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

;> (search-for-primes 10000000000000 3)
;10000000000037
;cpu time: 188 real time: 190 gc time: 0
;10000000000051
;cpu time: 188 real time: 195 gc time: 0
;10000000000099
;cpu time: 203 real time: 201 gc time: 0
;Done
;> (search-for-primes 100000000000000 3)
;100000000000031
;cpu time: 593 real time: 597 gc time: 0
;100000000000067
;cpu time: 610 real time: 599 gc time: 0
;100000000000097
;cpu time: 593 real time: 600 gc time: 0
;Done
;> (search-for-primes 1000000000000000 3)
;1000000000000037
;cpu time: 1922 real time: 1934 gc time: 16
;1000000000000091
;cpu time: 1859 real time: 1884 gc time: 0
;1000000000000159
;cpu time: 1875 real time: 1882 gc time: 0
;Done

;> (* 188 (sqrt 10))
;594.5082001116554
;> (* 593 (sqrt 10))
;1875.2306524798491