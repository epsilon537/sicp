#lang racket

;Exercise 3.63: Louis Reasoner asks why the sqrt-stream procedure was not written in the following more straightforward way, without the local variable guesses:
;
;(define (sqrt-stream x)
;  (cons-stream 
;   1.0
;   (stream-map (lambda (guess)
;                 (sqrt-improve guess x))
;               (sqrt-stream x))))
;
;Alyssa P. Hacker replies that this version of the procedure is considerably less efficient because it performs redundant computation. Explain Alyssa’s answer. Would the two ;versions still differ in efficiency if our implementation of delay used only (lambda () ⟨exp⟩) without using the optimization provided by memo-proc (3.5.1)?

;A: In the original version the next element in the sequence is based on the previously calculcated and memoized stream element. There is only one stream.
;In Lous Reasoner's version, sqrt-stream is invoked recursively, each invokation creating a new stream with a clean memo state. Memoization never kicks in.
;In an implementation without memoization, there would be no difference.
