#lang racket

(require racket/trace)

;Exercise 3.31: The internal procedure accept-action-procedure! defined in make-wire
;specifies that when a new action procedure is added to a wire, the procedure is
;immediately run. Explain why this initialization is necessary. In particular, trace
;through the half-adder example in the paragraphs above and say how the system’s response
;would differ if we had defined accept-action-procedure! as
;
;(define (accept-action-procedure! proc)
;  (set! action-procedures 
;        (cons proc action-procedures)))

(trace-define (make-agenda) 0)
(trace-define (empty-agenda? agenda) true)
(trace-define (first-agenda-item agenda) 0)
(trace-define (remove-first-agenda-item! agenda) 'ok)
(trace-define (add-to-agenda! time action agenda) 'ok)
(trace-define (current-time agenda) 0)

(trace-define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(trace-define (get-signal wire)
  (wire 'get-signal))
(trace-define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(trace-define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(trace-define (make-wire)
  (let ((signal-value 0) 
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each 
                  action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures 
            (cons proc action-procedures)))
    ;(define (accept-action-procedure! proc)
    ;  (set! action-procedures 
    ;        (cons proc action-procedures))
    ;  (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) 
             signal-value)
            ((eq? m 'set-signal!) 
             set-my-signal!)
            ((eq? m 'add-action!) 
             accept-action-procedure!)
            (else (error "Unknown operation: 
                          WIRE" m))))
    dispatch))

(trace-define (after-delay delay action)
  (add-to-agenda! 
   (+ delay (current-time the-agenda))
   action
   the-agenda))

(trace-define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item 
             (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(trace-define (probe name wire)
  (add-action! 
   wire
   (lambda ()
     (newline)
     (display name)
     (display " ")
     (display (current-time the-agenda))
     (display "  New-value = ")
     (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(trace-define (logical-or a b)
  (trace-define (in-range v)
    (or (= v 0) (= v 1)))
  (cond ((not (in-range a)) (error "Invalid signal" a))
        ((not (in-range b)) (error "Invalid signal" b))
        ((or (= a 1) (= b 1)) 1)
        (else 0)))

(trace-define (or-gate a1 a2 output)
  (trace-define (or-action-procedure)
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

(trace-define (logical-and a b)
  (trace-define (in-range v)
    (or (= v 0) (= v 1)))
  (cond ((not (in-range a)) (error "Invalid signal" a))
        ((not (in-range b)) (error "Invalid signal" b))
        ((and (= a 1) (= b 1)) 1)
        (else 0)))

(trace-define (and-gate a1 a2 output)
  (trace-define (and-action-procedure)
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

(trace-define (inverter input output)
  (trace-define (invert-input)
    (let ((new-value 
           (logical-not (get-signal input))))
      (after-delay 
       inverter-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(trace-define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(trace-define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)

(probe 'carry carry)

(display "invoking half-adder")
(newline)
(half-adder input-1 input-2 sum carry)
(display "half-addr invoked done.")
(newline)

(set-signal! input-1 1)

(propagate)

(set-signal! input-2 1)

(propagate)

;A: All the after-delay invokations are inside the action procedures. If the action procedure would not get called right away, the output ports' set-signal!s would never get scheduled. The initial input signal values would not propagate to the output. The output would stay at zero.

With original code (with initialization call):
(D,E,S and C are as in half-adder diagram)

in1=0, in2=0,D=0,E=0,S=0,C=0
agenda:
2: E->1
3: C->0, S->0
5: D->0

(set-signal! in1 1)

in1=1,in2=2,D=0,S=0,C=0
agenda:
2: E->1
3: C->0, S->0
5: D->0, D->1

(propagate)
in1=1,in2=0,D=0,E=1,S=0,C=0
agenda:
3: C->0, S->0
5: D->0, D->1, S->0

in1=1,in2=0,D=1,E=1,S=0,C=0
agenda:
5: D->0,D->1,S->0

in1=1,in2=0,D=1,E=1,S=0,C=0
agenda:
8: S->1

in1=1,in2=0,D=1,E=1,S=1,C=0
agenda:
/

With modified code (without initializer call):

in1=0,in2=0,D=0,E=0,S=0,C=0
agenda: /

(set-signal! in1 1)

in1=1,in2=0,D=0,E=0,S=0,C=0
agenda:
3: C->0
5: D->1

(propagate)

in1=1,in2=0,D=1,E=0,S=0,C=0
agenda:
5: D->1

in1=1,in2=0,D=1,E=0,S=0,C=0
agenda:
8: S->0

in1=1,in2=0,D=1,E=0,S=0,C=0
agenda:/

