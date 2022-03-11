#lang sicp

;Exercise 2.45: Right-split and up-split can be expressed as instances of a general splitting operation. Define a procedure split with the property that evaluating
;
;(define right-split (split beside below))
;(define up-split (split below beside))
;
;produces procedures right-split and up-split with the same behaviors as the ones already defined.


;(define (right-split painter n)
;  (if (= n 0)
;      painter
;      (let ((smaller (right-split painter 
;                                  (- n 1))))
;        (beside painter 
;                (below smaller smaller)))))

;(define (up-split painter n)
;  (if (= n 0)
;      painter
;      (let ((smaller (up-split painter 
;                               (- n 1))))
;        (below painter 
;                (beside smaller smaller)))))

(define (split opa opb)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split opa opb) painter 
                                        (- n 1))))
          (opa painter 
               (opb smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))
