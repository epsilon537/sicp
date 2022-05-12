#lang racket

;Exercise 3.32: The procedures to be run during each time segment of the agenda are kept in a queue. Thus, the procedures for each segment are called in the order in which they were
;added to the agenda (first in, first out). Explain why this order must be used. In particular, trace the behavior of an and-gate whose inputs change from 0, 1 to 1, 0 in the same
;segment and say how the behavior would differ if we stored a segment’s procedures in an ordinary list, adding and removing procedures only at the front (last in, first out).

;A:
;FIFO:
;in1=0,in2=1,out=0
;(set-signal! in1 1)
;in1=1,in2=1,out=0
;agenda: t=p-delay: out->1
;(set-signal! in2 0)
;in1=1,in2=0,out=0
;agenda: t=p-delay: out->1, out->0
;(propagate)
;in1=1,in2=0,out=0
;
;LIFO:
;in1=0,in2=1,out=0
;(set-signal! in1 1)
;in1=1,in2=1,out=0
;agenda: t=p-delay: out->1
;(set-signal! in2 0)
;in1=1,in2=0,out=0
;agenda: t=p-delay: out->0, out->1
;(propagate)
;in1=1,in2=0,out=0
