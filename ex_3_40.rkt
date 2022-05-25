#lang racket

;Exercise 3.40: Give all possible values of x that can result from executing
;
;(define x 10)
;(parallel-execute 
; (lambda () (set! x (* x x)))
; (lambda () (set! x (* x x x))))
;
;p1 = process 1
;p2 = process 2
;r1 = first read
;r2 = 2nd read
;r3 = 3rd read
;w = write
;
;p2r1,p2r2,p2r3,p2w,p1r1,p1r2,p1w
;x=10,10,10,1000,1000,1000,1000000
;(p2r1,p2r2,p2r3),p1r1,(p2r1,p2r2,p2r3),p2w,p1r2,p1w
;x=10,10,1000,1000,10000
;(p2r1,p2r2,p2r3),p1r1,(p2r1,p2r2,p2r3),p1r2,(p2r1,p2r2,p2r3),p2w,p1w
;x=10,10,10,1000,100
;(p2r1,p2r2,p2r3),p1r1,(p2r1,p2r2,p2r3),p1r2,(p2r1,p2r2,p2r3),p1w,p2w
;x=10,10,10,100,1000
;(p2r1,p2r2),p1r1,(p2r1,p2r2),p1r2,(p2r1,p2r2),p1w,p2r3,p2w
;x=10,10,10,100,100,10000
;(p2r1),p1r1,(p2r1),p1r2,(p2r1),p1w,p2r2,p2r3,p2w
;x=10,10,10,100,100,100,100000
;p1r1,p1r2,p1w,p2r1,p2r2,p2r3,p2w
;x=10,10,100,100,100,100,1000000

;Which of these possibilities remain if we instead use serialized procedures:
;
;(define x 10)
;(define s (make-serializer))
;(parallel-execute 
; (s (lambda () (set! x (* x x))))
; (s (lambda () (set! x (* x x x)))))
;
;p1r1,p1r2,p1w,p2r1,p2r2,p2r3,p2w
;x=10,10,100,100,100,100,1000000
;p2r1,p2r2,p2r3,p2w,p1r1,p1r2,p1w
;x=10,10,10,1000,1000,1000,1000000
