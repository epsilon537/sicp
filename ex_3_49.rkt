#lang racket

;Exercise 3.49: Give a scenario where the deadlock-avoidance mechanism described above does not work. (Hint: In the exchange problem, each process knows in advance which accounts it will
;need to get access to. Consider a situation where a process must get access to some shared resources before it can know which additional shared resources it will require.)

;A: Inserting an item in a double linked list.
;E.g A<->B
;Process 1 wants to insert C after A, while process 2 wants to insert D before B:
;
;(define (insert-after node item)
;  ...)
;
;(define (insert-before node item)
;  ...)
