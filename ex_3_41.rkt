#lang racket

;Exercise 3.41: Ben Bitdiddle worries that it would be better to implement the bank
;account as follows (where the commented line has been changed):
;
;(define (make-account balance)
;  (define (withdraw amount)
;    (if (>= balance amount)
;        (begin 
;          (set! balance 
;                (- balance amount))
;          balance)
;        "Insufficient funds"))
;  (define (deposit amount)
;    (set! balance (+ balance amount))
;    balance)
;  (let ((protected (make-serializer)))
;    (define (dispatch m)
;      (cond ((eq? m 'withdraw) 
;             (protected withdraw))
;            ((eq? m 'deposit) 
;             (protected deposit))
;            ((eq? m 'balance)
;             ((protected 
;                (lambda () 
;                  balance)))) ; serialized
;            (else 
;             (error 
;              "Unknown request: 
;               MAKE-ACCOUNT"
;              m))))
;    dispatch))
;
;because allowing unserialized access to the bank balance can result in anomalous ;behavior. Do you agree? Is there any scenario that demonstrates Ben’s concern?

;A: I don't agree. There's no point in serializing a single 'atomic' operation such as a ;variable read operation. Serialized or not, invoking 'balance on the object just returns ;its momentary value. It might change immediately afterward by a 'withdraw or a 'deposit. ;Protecting 'balance won't change that.
;If balance were not an 'atomic' value (e.g. a list), it would be possible to read a ;partially written value. In that case protecting it makes sense.
