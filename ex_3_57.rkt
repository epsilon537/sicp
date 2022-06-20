#lang racket

;Exercise 3.57: How many additions are performed when we compute the n th Fibonacci number using the definition of fibs based on the add-streams procedure? Show that the number
;of additions would be exponentially greater if we had implemented (delay ⟨exp⟩) simply as (lambda () ⟨exp⟩), without using the optimization provided by the
;memo-proc procedure described in 3.5.1.192

;With memoization: Each step introduces one addition and reliese on the results of previous computations otherwise.
;The first two elements require no additions -> the nth element requires (n-2) additions.
;E.g 3 additions for the 5th element.
;
;Without memoization:
;
;f(0) -> f(1) -> f(2) -> f(3) -> / f(4)
;                 +       +     /  +
;             f(1)f(0) f(2)(f1)/ f(3)f(2)
;                       +     /    +    +
;                  f(1)f(0)  / f(2)(f1) f(1)f(0)
;                           /  +
;                          /f(1)f(0)
;
;The number of additions for step Xn = 1 + 2Xn-1
;E.g. 7 additions for the 5th element.
;
;More than a doubling, each step -> exponential growth.
