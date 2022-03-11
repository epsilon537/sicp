#lang sicp

;Exercise 2.55: Eva Lu Ator types to the interpreter the expression
;
;(car ''abracadabra)
;
;To her surprise, the interpreter prints back quote. Explain.
;
;(car ''abracadabra)
;is equivalent to:
;(car (quote (quote abracadabra)))
;i.e. we have a list consisting of two symbols: quote and abracadbra. car returns the first element, quote.
