#lang sicp

;Exercise 1.34: Suppose we define the procedure
;
;(define (f g) (g 2))
;
;Then we have
;
;(f square)
;4
;
;(f (lambda (z) (* z (+ z 1))))
;6
;
;What happens if we (perversely) ask the interpreter to evaluate the combination (f f)? Explain.
;
;(define (f g) (g 2))
;
;is equivalent to:
;
;(define f (lambda (g) (g 2)))
;
;(f f)
;
;is equivalent to:
;((lambda (g) (g 2)) (lambda (g) (g 2)))
;
;Substituting:
;((lambda (g) (g 2)) 2)
;
;Substituting:
;(2 2)
;
;-> Error
 