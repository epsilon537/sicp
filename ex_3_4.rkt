#lang racket

;Exercise 3.4: Modify the make-account procedure of Exercise 3.3 by adding another local state variable so that, if an account is accessed more than seven consecutive times with an
;incorrect password, it invokes the procedure call-the-cops.

(define MAX_NUM_INCORRECT_PASSWORDS 7)

(define (call-the-cops)
  "Calling the cops")

(define (make-account balance secret-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  
  (define (deposit amount)
    (set! balance (+ balance amount)) balance)

  (let ((num-consec-incorrect-passwords 0))
    (define (dispatch password m)
      (if (not (eq? password secret-password))
          (lambda (x)
            (set! num-consec-incorrect-passwords (+ num-consec-incorrect-passwords 1))
            (if (> num-consec-incorrect-passwords MAX_NUM_INCORRECT_PASSWORDS)
                (call-the-cops)
                "Incorrect password"))
          (begin
            (set! num-consec-incorrect-passwords 0)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request: MAKE-ACCOUNT" m))))))
    dispatch))

(define acc 
  (make-account 100 'secret-password))

((acc 'some-other-password 'withdraw) 10)
((acc 'some-other-password 'withdraw) 10)
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'withdraw) 10)
((acc 'some-other-password 'withdraw) 10)
((acc 'some-other-password 'withdraw) 10)
((acc 'some-other-password 'withdraw) 10)
((acc 'some-other-password 'withdraw) 10)
((acc 'some-other-password 'withdraw) 10)
((acc 'some-other-password 'withdraw) 10)
((acc 'some-other-password 'withdraw) 10)
