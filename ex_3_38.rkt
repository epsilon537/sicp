#lang racket

;Exercise 3.38: Suppose that Peter, Paul, and Mary share a joint bank account that ;initially contains $100. Concurrently, Peter deposits $10, Paul withdraws $20, and Mary
;withdraws half the money in the account, by executing the following commands:
;
;Peter: (set! balance (+ balance 10))
;Paul:  (set! balance (- balance 20))
;Mary:  (set! balance (- balance 
;                        (/ balance 2)))
;
;    List all the different possible values for balance after these three transactions
;have been completed, assuming that the banking system forces the three processes to run
;sequentially in some order.
;    What are some other values that could be produced if the system allows the processes
;to be interleaved? Draw timing diagrams like the one in Figure 3.29 to explain how these
;values can occur.

1. Peter->Paul->Mary: ((balance+10)-20)/2 = balance/2 - 5
   Mary->Peter->Paul: ((balance/2)+10)-20 = balance/2 - 10
   Paul->Mary->Peter: ((balance-20)/2)+10 = balance/2
   Mary->Paul->Peter: ((balance/2)-20)+10 = balance/2 - 10
   Peter->Mary->Paul: ((balance+10)/2)-20 = balance/2 - 15
   Paul->Peter->Mary: ((balance-20)+10)/2 = balance/2 - 5

2. Paul-Read, Paul-Write, Peter-Read, Peter-Write, Mary-Read, Mary-Write.
There are 6! permutations of these events. However, half of these 6! permutations are invalid because Mary writes before she reads. Half the remaining permutations are invalid because Peter writes before he reads. Half of those remaining permutations are invalid because Paul writes before he reads => There are 6!/(2^3) = 90 valid permutations.

Some examples:

Paul-Read -> Peter-Read -> Paul-Write -> Mary-Read -> Mary-Write -> Peter Write
balance      balance       (balance-20)  (balance-20) (balance-20)/2 (balance+10)

Mary-Read -> Mary-Write -> Paul-Read -> Peter-Read -> Paul-Write -> Peter Write
balance      (balance/2)   (balance/2)  (balance/2)   (balance/2 - 20) (balance/2 + 10)

Peter-Read -> Paul-Read -> Paul-Write -> Peter-Write -> Mary-Read -> Mary-Write
balance       balance      (balance-20)  (balance+10)   (balance+10) (balance+10)/2



