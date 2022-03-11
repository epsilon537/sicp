#lang sicp

;Exercise 1.25: Alyssa P. Hacker complains that we went to a lot of extra work in writing expmod. After all, she says, since we already know how to compute exponentials, we could have
;simply written
;
;(define (expmod base exp m)
;  (remainder (fast-expt base exp) m))
;
;Is she correct? Would this procedure serve as well for our fast prime tester? Explain.

;Alyssa's version would work but computes huge internal values (n^n) while the original version, due to the modulo operation, never has values exceeding n.
;The arbitrary precision logic in Scheme would accept the huge internal values in Alyssa's version, but it would be slow.
