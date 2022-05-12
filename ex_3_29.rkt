#lang racket

;Exercise 3.29: Another way to construct an or-gate is as a compound digital logic device, built from and-gates and inverters. Define a procedure or-gate that accomplishes this. What is ;the delay time of the or-gate in terms of and-gate-delay and inverter-delay?

(define (or-gate a1 a2 output)
  (let ((i1 (make-wire))
        (i2 (make-wire))
        (o1 (make-wire)))
    (inverter a1 i1)
    (inverter a2 i2)
    (and-gate i1 i2 o1)
    (inverter o1 output)))

;A: the delay time is 2 inverter-delays and 1 and-gate-delay

            