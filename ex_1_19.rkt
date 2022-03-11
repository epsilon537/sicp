#lang sicp

;Exercise 1.19: There is a clever algorithm for computing the Fibonacci numbers in a logarithmic number of steps. Recall the transformation of the state variables a and b in the fib-iter
;process of 1.2.2: a ← a + b and b ← a . Call this transformation T , and observe that applying T over and over again n times, starting with 1 and 0, produces the pair Fib ( n + 1 ) and
;Fib ( n ) . In other words, the Fibonacci numbers are produced by applying T n , the n th power of the transformation T , starting with the pair (1, 0). Now consider T to be the special
;case of p = 0 and q = 1 in a family of transformations T p q , where T p q transforms the pair ( a , b ) according to a ← b q + a q + a p and b ← b p + a q . Show that if we apply such
;a transformation T p q twice, the effect is the same as using a single transformation T p ′ q ′ of the same form, and compute p ′ and q ′ in terms of p and q . This gives us an explicit
;way to square these transformations, and thus we can compute T n using successive squaring, as in the fast-expt procedure. Put this all together to complete the following procedure,
;which runs in a logarithmic number of steps:
;
;(define (fib n)
;  (fib-iter 1 0 0 1 n))
;
;(define (fib-iter a b p q count)
;  (cond ((= count 0) 
;         b)
;        ((even? count)
;         (fib-iter a
;                   b
;                   ;compute p'
;                   ;compute q'
;                   (/ count 2)))
;        (else 
;         (fib-iter (+ (* b q) 
;                      (* a q) 
;                      (* a p))
;                   (+ (* b p) 
;                      (* a q))
;                   p
;                   q
;                   (- count 1)))))

;(Tpq)^2:
;an = bn-1*q + an-1*q + an-1*p = bn-1*q +an-1*(p+q)
;bn = bn-1*p + an-1*q
;an-1 = bn-2*q+an-2*q+an-2*p
;bn-1 = bn-2*p+an-2*q
;
;...
;
;an = bn-2(2pq+q^2)+an-2(2q^2+2pq+p^2)
;bn = bn-2(p^2+q^2)+an-2(2pq+q^2)
;
;=>
;
;q' = 2pq+q^2 <===============
;p'+q' = 2q^2 + 2pq+p^2
;p' = p^2+q^2 <===============
;q' = 2pq+q^2


(define (square a)
  (* a a))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) 
         b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else 
         (fib-iter (+ (* b q) 
                      (* a q) 
                      (* a p))
                   (+ (* b p) 
                      (* a q))
                   p
                   q
                   (- count 1)))))