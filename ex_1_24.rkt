#lang racket

(define (square a)
  (* a a))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (fast-prime? n 1000000)) ;test 1000000 times to slow things down

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

> (search-for-primes 1000 3)
1009
cpu time: 1281 real time: 1284 gc time: 15
1013
cpu time: 1297 real time: 1300 gc time: 0
1019
cpu time: 1328 real time: 1330 gc time: 15
Done
> (search-for-primes 1000000 3)
1000003
cpu time: 2062 real time: 2076 gc time: 15
1000033
cpu time: 2172 real time: 2173 gc time: 0
1000037
cpu time: 2078 real time: 2085 gc time: 0 -> ratio: 1.6, expected 2.0
Done
> (search-for-primes 1000000000 3)
1000000007
cpu time: 3188 real time: 3204 gc time: 15
1000000009
cpu time: 3109 real time: 3112 gc time: 16
1000000021
cpu time: 3406 real time: 3411 gc time: 32 -> ratio: 1.6, expected 1.5
Done
> 