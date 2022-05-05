#lang racket

;Exercise 3.30: Figure 3.27 shows a ripple-carry adder formed by stringing together n full-adders. This is the simplest form of parallel adder for adding two n -bit binary numbers. The
;inputs A 1 , A 2 , A 3 , ..., A n and B 1 , B 2 , B 3 , ..., B n are the two binary numbers to be added (each A k and B k is a 0 or a 1). The circuit generates S 1 , S 2 , S 3 , ..., S
;n , the n bits of the sum, and C , the carry from the addition. Write a procedure ripple-carry-adder that generates this circuit. The procedure should take as arguments three lists of n
;wires each—the A k , the B k , and the S k —and also another wire C . The major drawback of the ripple-carry adder is the need to wait for the carry signals to propagate. What is the
;delay needed to obtain the complete output from an n -bit ripple-carry adder, expressed in terms of the delays for and-gates, or-gates, and inverters?

(define (get-signal a)
  'placeholder)

(define (after-delay d f)
  'placeholder)

(define or-gate-delay 0)
(define and-gate-delay 0)
(define inverter-delay 0)

(define (set-signal! a v)
  'placeholder)

(define (add-action! s a)
  'placeholder)

(define (make-wire)
  'placeholder)

(define (inverter input output)
  (define (invert-input)
    (let ((new-value 
           (logical-not (get-signal input))))
      (after-delay 
       inverter-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and a b)
  (define (in-range v)
    (or (= v 0) (= v 1)))
  (cond ((not (in-range a)) (error "Invalid signal" a))
        ((not (in-range b)) (error "Invalid signal" b))
        ((and (= a 1) (= b 1)) 1)
        (else 0)))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) 
                        (get-signal a2))))
      (after-delay 
       and-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-or a b)
  (define (in-range v)
    (or (= v 0) (= v 1)))
  (cond ((not (in-range a)) (error "Invalid signal" a))
        ((not (in-range b)) (error "Invalid signal" b))
        ((or (= a 1) (= b 1)) 1)
        (else 0)))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) 
                        (get-signal a2))))
      (after-delay 
       or-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((c1 (make-wire)) 
        (c2 (make-wire))
        (s  (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder ak bk sk c)
  (if (empty? ak)
      'ok
      (let ((c-out (make-wire)))
        (full-adder (car ak) (car bk) c (car sk) c-out)
        (ripple-carry-adder (cdr ak) (cdr bk) (cdr sk) c-out))))

;A:
;n-bit ripple-carry-adder delay = (n-1)*full-adder-carry + full-adder sum delay
;full-adder-carry delay = half-adder-sum + half-adder-carry + or-gate delay
;full-adder-sum delay = 2*half-adder-sum delay
;half-adder-sum delay = and-gate + max(inverter + and-gate, or-gate) delay
;half-adder carry delay = and-gate delay
;
;=>
;
;n-bit ripple-carry-adder delay =
;(n-1)*(half-adder-sum + half-adder carry + or-gate) + 2*half-adder sum delay =
;(n-1)*(and-gate+max() + and-gate + or-gate) + 2*and-gate + 2*max() delay =
;
;(n+1)*and-gate+(n+1)*max(inverter + and-gate, or-gate)+(n-1)*or-gate delay


