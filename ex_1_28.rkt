#lang sicp

(define (square a)
  (* a a))

(define (expmod base exp m)
  ;The let special form has not been introduced yet at this point in the text, so we have to jump through some hoops to avoid using that.
  ;Using a nested function with two arguments, v and its sqrmod, does the trick.
  (define (non-trivial-root-check v vsqrmod) ; non-trivial root modulo m check. Returns sqrmod value if not a root, 0 if a root.
    (cond ((= v 1) vsqrmod) ; if 1 -> not a non-trival root -> return sqrmod value
          ((= v (- m 1)) vsqrmod) ; if (m - 1) -> not a non-trival-root -> return sqrmod value.
          ((= vsqrmod 1) 0) ; if sqrmod value = 1 -> non-trivial-root -> return 0.
          (else vsqrmod) ; else: not a non-trival-root -> return sqrmod value.
          ))

  (define (squaremod-checked v) ; Perform (v^2)%m, but if result is a non-trivial root modulo m, return 0 instead of computed value.
    (non-trivial-root-check v (remainder (square v) m)))
  
  (cond ((= exp 0) 1)
        ((even? exp)
         (squaremod-checked (expmod base (/ exp 2) m))) 
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 561 1000)
(fast-prime? 1105 1000)
(fast-prime? 1729 1000)
(fast-prime? 2465 1000)
(fast-prime? 2821 1000)
(fast-prime? 6601 1000)