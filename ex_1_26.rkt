#lang sicp

;Exercise 1.26: Louis Reasoner is having great difficulty doing Exercise 1.24. His fast-prime? test seems to run more slowly than his prime? test. Louis calls his friend Eva Lu Ator over
;to help. When they examine Louis’s code, they find that he has rewritten the expmod procedure to use an explicit multiplication, rather than calling square:
;
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (* (expmod base (/ exp 2) m)
             (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base 
             (expmod base (- exp 1) m))
          m))))

;“I don’t see what difference that could make,” says Louis. “I do.” says Eva. “By writing the procedure like that, you have transformed the Θ ( log ⁡ n ) process into a Θ ( n ) process.”
;Explain.

;We get tree recursion instead of iteration. The tree grows (each node doubles) exponentially with depth. The depth of the tree is log(n). the log(n) depth and exponential growth
;of the tree recursion cancel each other out to a number of steps linear with N.
