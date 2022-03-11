#lang sicp

;Exercise 2.15: Eva Lu Ator, another user, has also noticed the different intervals computed by different but algebraically equivalent expressions. She says that a formula to compute ;with intervals using Alyssa’s system will produce tighter error bounds if it can be written in such a form that no variable that represents an uncertain number is repeated. Thus, she ;says, par2 is a “better” program for parallel resistances than par1. Is she right? Why?
;
;p(ab) = p(a) + p(b)
;p(a/b) = p(a) + p(b)
;p(1/a) = p(a)
;
;w(a+b) = w(a) + w(b)
;p(a) = w(a)/a
;=> p(a+b) = w(a+b)/(a+b)= (a/(a+b))p(a) + (b/(a+b))p(b)
;
;p(ab/(a+b)) = p(ab) + p(a+b) = ... = (1+a/(a+b))p(a) + (1+b/(a+b))p(b)
;
;p(1/(1/a+1/b)) = p(1/a+1/b) = p(a+b) = (a/(a+b))p(a) + (b/(a+b))p(b)
;
;p(ab/(a+b)) - p(1/(1/a+1/b)) = pa + pb > 0 => Yes.
