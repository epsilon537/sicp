#lang sicp

;Exercise 4.15: Given a one-argument procedure p and an object a, p is said to “halt” on a if evaluating the expression (p a) returns a value (as opposed to terminating with an
;error message or running forever). Show that it is impossible to write a procedure halts? that correctly determines whether p halts on a for any procedure p and object a. Use
;the following reasoning: If you had such a procedure halts?, you could implement the following program:
;
;(define (run-forever)
;  (run-forever))
;
;(define (try p)
;  (if (halts? p p)
;      (run-forever)
;      'halted))
;
;Now consider evaluating the expression (try try) and show that any possible outcome (either halting or running forever) violates the intended behavior of halts?.227

;A:
;If we had a 'halts?', it would return true if (try try) returns a value and false otherwise.
;=>
;- if (try try) returns a value, (halts? try try) should return true.
;- if (try try) does not return a value (halts? try try), should return false.
;Assuming this is the assumed behavior of 'halts?', the try implementation reverses the behavior:
;- if (halts? try try) returns true (indicating (try try) returns a value), (try try) does not return a value.
;- if (halts? try try) returns false (indicating (try try) does not return a value), (try try) returns a value.
;This is a logic contradiction => The premise, 'halts?' exists, is false.
