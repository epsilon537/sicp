#lang racket

(define (memo-proc proc)
  (let ((already-run? false)
        (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define-syntax delay
  (syntax-rules ()
    ((delay exp)
     (memo-proc (lambda () exp)))))

(define (force delayed-object)
  (delayed-object))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define stream-null? null?)
(define the-empty-stream '())
(define nil '())

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc 
                    (map stream-cdr 
                         argstreams))))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) 
         the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream 
          (stream-car stream)
          (stream-filter 
           pred
           (stream-cdr stream))))
        (else (stream-filter 
               pred 
               (stream-cdr stream)))))

(define (display-stream-n s n)
  (if (= n 0)
      "Done."
      (begin
        (display (stream-car s))
        (newline)
        (display-stream-n (stream-cdr s) (- n 1)))))

(define (add-streams s1 s2) 
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

;    Exercise 3.80: A series RLC circuit consists of a resistor, a capacitor, and an inductor connected in series, as shown in Figure 3.36. If R , L , and C are the resistance,
;inductance, and capacitance, then the relations between voltage ( v ) and current ( i ) for the three components are described by the equations
;v R = i R R , v L = L d i L d t , i C = C d v C d t ,
;and the circuit connections dictate the relations
;i R = i L = - i C , v C = v L + v R .
;Combining these equations shows that the state of the circuit (summarized by v C , the voltage across the capacitor, and i L , the current in the inductor) is described by the
;pair of differential equations
;d v C d t = - i L C , d i L d t = 1 L v C - R L i L .
;The signal-flow diagram representing this system of differential equations is shown in Figure 3.37. 
;
;SVG
;
;Figure 3.37: A signal-flow diagram for the solution to a series RLC circuit.
;
;    Write a procedure RLC that takes as arguments the parameters R , L , and C of the circuit and the time increment d t . In a manner similar to that of the RC procedure of
;Exercise 3.73, RLC should produce a procedure that takes the initial values of the state variables, v C 0 and i L 0 , and produces a pair (using cons) of the streams of states
;vC and i L . Using RLC, generate the pair of streams that models the behavior of a series RLC circuit with R = 1 ohm, C = 0.2 farad, L = 1 henry, d t = 0.1 second, and initial
;values i L 0 = 0 amps and v C 0 = 10 volts.

(define (integral
         delayed-integrand initial-value dt)
  (define int
    (cons-stream 
     initial-value
     (let ((integrand 
            (force delayed-integrand)))
       (add-streams 
        (scale-stream integrand dt)
        int))))
  int)

(define (RLC R L C dt)
  (define (RLC-proc vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dvc (scale-stream il (/ (- 1) C)))
    (define dil (add-streams (scale-stream il (/ (- R) L)) (scale-stream vc (/ 1 L))))
    (cons vc il)
  )
  RLC-proc)

(define RLC-circuit (RLC 1 1 0.2 0.1))

(display-stream-n (car (RLC-circuit 10 0)) 100)
(display-stream-n (cdr (RLC-circuit 10 0)) 100)