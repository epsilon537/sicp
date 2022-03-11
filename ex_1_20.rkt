#lang sicp

;Exercise 1.20: The process that a procedure generates is of course dependent on the rules used by the interpreter. As an example, consider the iterative gcd procedure given above.
;Suppose we were to interpret this procedure using normal-order evaluation, as discussed in 1.1.5. (The normal-order-evaluation rule for if is described in Exercise 1.5.) Using the
;substitution method (for normal order), illustrate the process generated in evaluating (gcd 206 40) and indicate the remainder operations that are actually performed. How many remainder
;operations are actually performed in the normal-order evaluation of (gcd 206 40)? In the applicative-order evaluation?
;
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;Normal order evaluation:
;(gcd 206 40)
;(if (= 40 0)
;      206
;      (gcd 40 (remainder 206 40))))
;(if (= 6 0)
;      40
;      (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))))
;(if (= 4 0)
;      (remainder 206 40)
;      (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
;(if (= 2 0)
;      (remainder 40 (remainder 206 40))
;      (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 ;40)))))))
;(if (= 0 0)
;      2
;      (gcd b (remainder a b))))
;
;remainders performed: IIIIIIIIIIIIIIIIII = 18
;
;Applicative order evaluation:
;(gcd 206 40)
;(if (= 40 0)
;      a
;      (gcd 40 6)))
;(if (= 6 0)
;      a
;      (gcd 6 4)))
;(if (= 4 0)
;      a
;      (gcd 4 2)))
;(if (= 2 0)
;      a
;      (gcd 2 0)))
;(if (= 0 0)
;      2
;      (gcd b (remainder a b))))
;remainders performed: IIII = 4
