#lang sicp

;Exercise 2.13: Show that under the assumption of small percentage tolerances there is a simple formula for the approximate percentage tolerance of the product of two intervals in terms
;of the tolerances of the factors. You may simplify the problem by assuming that all numbers are positive.

la = lower a, lb = lower b, ua = upper a, ub = upper b, lab = lower ab, uab = upper ab

la = a(1-pa)
lb = b(1-pb)
ua = a(1+pa)
ub = b(1+pb)
lab = lalb = a(1-pa)b(1-pb)
uab = uaub = a(1+pa)b(1+pb)
pab = wab/(ab) = (uab-lab)/(2ab) = (a(1+pa)b(1+pb) - a(1-pa)b(1-pb))/(2ab) = ... = pa + pb - papb
papb ~= 0 => pab ~= pa + pb

