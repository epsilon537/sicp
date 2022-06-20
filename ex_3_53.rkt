#lang racket

;Exercise 3.53: Without running the program, describe the elements of the stream defined by
;
;(define s (cons-stream 1 (add-streams s s)))

First element:
(stream-car s)
(car (cons 1 (delay add-streams...)))
-> 1

2nd element:
(stream-car (stream-cdr s))
(stream-car (force (cdr (cons 1 (delay add-streams...)))))
(stream-car (force (delay add-streams...)))
(stream-car (add-streams s s))
(stream-car (stream-map + s s))
(stream-car (cons (+ (stream-car s) (stream-car s))
                  (delay stream-map...)))
(stream-car (cons (+ (stream-car (cons 1 (delay add-streams...))) (stream-car (cons 1 (delay add-streams...))))
                  (delay stream-map...)))
(stream-car (cons (+ 1 1)
                  (delay stream-map...)))
-> 2

3rd element:
(stream-car (stream-cdr (stream-cdr s)))
(stream-car (stream-cdr (force (cdr (cons 1 (delay add-streams...))))))
(stream-car (stream-cdr (force (delay add-streams...))))
(stream-car (stream-cdr (add-streams s s)))          
(stream-car (stream-cdr (stream-map + s s)))
(stream-car (stream-cdr (cons (+ (stream-car s) (stream-car s))
                              (delay stream-map...))))
(stream-car (force (delay stream-map...)))
(stream-car (stream-map + (cons (+ 1 1) (delay stream-map...)) (cons (+ 1 1) (delay stream-map...))))
(stream-car (stream-map + (cons 2 (delay stream-map...)) (cons 2 (delay stream-map...))))
(stream-car (cons (+ (stream-car (cons 2 (delay stream-map...))) (stream-car (cons 2 (delay stream-map...))))
                              (delay stream-map...)))
(stream-car (cons (+ 2 2)
                  (delay stream-map...)))
-> 4
...