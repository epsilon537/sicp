#lang racket

;Exercise 3.7: Consider the bank account objects created by make-account, with the password modification described in Exercise 3.3. Suppose that our banking system requires the ability
;to make joint accounts. Define a procedure make-joint that accomplishes this. Make-joint should take three arguments. The first is a password-protected account. The second argument must
;match the password with which the account was defined in order for the make-joint operation to proceed. The third argument is a new password. Make-joint is to create an additional
;access to the original account using the new password. For example, if peter-acc is a bank account with password open-sesame, then
;
;(define paul-acc
;  (make-joint peter-acc 
;              'open-sesame 
;              'rosebud))
;
;will allow one to make transactions on peter-acc using the name paul-acc and the password rosebud. You may wish to modify your solution to Exercise 3.3 to accommodate this new feature.

(define (make-account balance secret-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  
  (define (deposit amount)
    (set! balance (+ balance amount)) balance)

  (define (check-password pw)
    (eq? pw secret-password))
  
  (define (dispatch password m)
    (if (eq? m 'check-password)
        (check-password password)
        (if (not (check-password password))
            (lambda (x) "Incorrect password")
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request: MAKE-ACCOUNT" m))))))
  dispatch)

(define (make-joint account orig-pw joint-pw)
  (define (dispatch password m)
    (if (not (eq? password joint-pw))
        (lambda (x) "Incorrect joint account password")
        (account orig-pw m)))
  
  (if (not (account orig-pw 'check-password))
      "Incorrect base account password"
      dispatch))

(define peter-acc (make-account 100 'open-sesame))

(define paul-acc
  (make-joint peter-acc 
              'open-sesame 
              'rosebud))

(make-joint peter-acc 'wrong-password 'irrelevant-password)

((peter-acc 'open-sesame 'withdraw) 40)
((paul-acc 'rosebud 'withdraw) 40)
((peter-acc 'open-sesame 'deposit) 50)
((paul-acc 'rosebud 'deposit) 50)
((paul-acc 'wrong-password 'deposit) 50)
