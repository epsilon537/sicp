#lang sicp

; 2.19: Consider the change-counting program of 1.2.2. It would be nice to be able to easily change the currency used by the program, so that we could compute the number of ways to
;change a British pound, for example. As the program is written, the knowledge of the currency is distributed partly into the procedure first-denomination and partly into the procedure
;count-change (which knows that there are five kinds of U.S. coins). It would be nicer to be able to supply a list of coins to be used for making change.

;We want to rewrite the procedure cc so that its second argument is a list of the values of the coins to use rather than an integer specifying which coins to use. We could then have
;lists that defined each kind of currency:

(define us-coins 
  (list 50 25 10 5 1))

(define uk-coins 
  (list 100 50 20 10 5 2 1 0.5))

;We could then call cc as follows:
;
;(cc 100 us-coins)
;292
;
;To do this will require changing the program cc somewhat. It will still have the same form, but it will access its second argument differently, as follows:

(define (cc amount coin-values)
  (cond ((= amount 0) 
         1)
        ((or (< amount 0) 
             (no-more? coin-values)) 
         0)
        (else
         (+ (cc 
             amount
             (except-first-denomination 
              coin-values))
            (cc 
             (- amount
                (first-denomination 
                 coin-values))
             coin-values)))))

;Reduction step: sum of
;(1) nbr-of-ways-to-make-change with all except first denomination in list and
;(2) nbr-of-way-to-make-change with all denominations with amount reduced by first-denomination.

;Define the procedures first-denomination, except-first-denomination and no-more? in terms of primitive operations on list structures. Does the order of the list coin-values affect the
;answer produced by cc? Why or why not?

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(cc 100 us-coins)
(cc 100 uk-coins)

;The modified algorithms works the same way as the original algorithm, going through the given denominations in the same order as the original algorithm. In the original algorithm,
;the order of denominations doesn't matter (as stated in 1.1.2), so neither does it here. We essentially just took a code-driven combination from the algorithm (the cond expression in
;first-denomination) and made it data driven. The behavior of the algorithm itself hasn't changed.
;In the original algorithm (and thus the current one), the order doesn't matter because the algorithm scans through all ways to make change and 'all ways to make change' is independent
;of the order of denominations used.

(define us-coins-2 
  (list 1 5 10 25 50))

(define uk-coins-2 
  (list 0.5 1 2 5 10 20 50 100))

(cc 100 us-coins-2)
(cc 100 uk-coins-2)
