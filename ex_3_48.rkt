#lang racket

(require rnrs/mutable-pairs-6) ;for set-car! and set-cdr!
(require compatibility/mlist) ;for mlist

(define cons mcons)
(define car mcar)
(define cdr mcdr)
(define (cadr x) (car (cdr x)))
(define list mlist)

;Exercise 3.48: Explain in detail why the deadlock-avoidance method described above, (i.e., the accounts are numbered, and each process attempts to acquire the smaller-numbered account
;first) avoids deadlock in the exchange problem. Rewrite serialized-exchange to incorporate this idea. (You will also need to modify make-account so that each account is created with a
;number, which can be accessed by sending an appropriate message.)

;Deadlock involving acquisition of two protected resources occurs when one process gets one protected resource and blocks trying to get the other protected resource while another process
;gets the other protected resource and blocks trying to get the one protected resource. e.g. the following sequence with processes P1 and P2 and protected resources R1 and R2:
;P1 and P2 both want exclusive access to R1 and R2. P1 tries to acquire R1 first, then R2. P2 tries to acquire R2 first, then R1:
;P1 gets R1
;P2 gets R2
;P1 blocks trying to get R2 (because P2 has it)
;P2 blocks trying to get R1 (because P1 has it)
;
;If both process always acquire access to the two shared resources in the same order (e.g. R1 before R2), you can't end up in a situation where a process has one resources and can't get the other. Getting R2 depends on successfully getting R1 first.
;P1 gets R1
;P2 blocks trying to get R1
;P1 gets R2
;P1 releases R2
;P1 relases R1
;P2 gets R1
;P2 gets R2
;P2 releases R2
;P2 releases R1
;or:
;P1 gets R1
;P1 gets R2
;P2 blocks trying to get R1
;P1 releases R2
;P1 releases R1
;P2 gets R1
;P2 gets R2
;P2 releases R2
;P2 releases R1
;or:
;P1 gets R1
;P1 gets R2
;P1 releases R2
;P2 blocks trying to get R1
;P1 releases R1
;P2 gets R1
;P2 gets R2
;P2 releases R2
;P2 releases R1
;...

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire) ; retry
                 '())) 
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell) (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-account-and-serializer balance account-num)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin 
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer 
         (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) 
             balance-serializer)
            ((eq? m 'account-number)
             account-num)
            (else (error "Unknown request: 
                          MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((ordered-accounts (if (< (account1 'account-number) (account2 'account-number))
                              (list account1 account2)
                              (list account2 account1))))
    (let ((low-account (car ordered-accounts))
          (high-account (cadr ordered-accounts)))
      (let ((serializerl (low-account 'serializer))
            (serializerh (high-account 'serializer)))
        ((serializerl (serializerh exchange))
         low-account
         high-account)))))

(define (make-make-account)
  (let ((account-num 0)
        (mutex (make-mutex)))
    (define (allocate-account-num)
      (mutex 'acquire)
      (set! account-num (+ account-num 1))
      (let ((allocated-account-num account-num))
        (mutex 'release)
        allocated-account-num))
    (lambda (balance)
      (make-account-and-serializer balance (allocate-account-num)))))

(define make-account (make-make-account))
(define acc1 (make-account 100))
(define acc2 (make-account 200))

