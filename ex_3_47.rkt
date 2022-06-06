#lang racket

(require rnrs/mutable-pairs-6) ;for set-car! and set-cdr!
(require compatibility/mlist) ;for mlist

(define cons mcons)
(define car mcar)
(define cdr mcdr)
(define list mlist)

;Exercise 3.47: A semaphore (of size n ) is a generalization of a mutex. Like a mutex, a semaphore supports acquire and release operations, but it is more general in that up to n
;processes can acquire it concurrently. Additional processes that attempt to acquire the semaphore must wait for release operations. Give implementations of semaphores
;
;    in terms of mutexes
;    in terms of atomic test-and-set! operations.

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

(define (make-semaphore-m n)
  (let ((mutex (make-mutex))
        (val n))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire)
             (if (> val 0)
                 (begin
                   (set! val (- val 1))
                   (mutex 'release))
                 (begin
                   (mutex 'release)
                   (the-semaphore 'acquire))))
            ((eq? m 'release)
             (mutex 'acquire)
             (if (< val n)
                 (set! val (+ val 1))
                 '())
             (mutex 'release))
            ((eq? m 'value)
             val)))
    the-semaphore))

(define (make-semaphore-t n)
  (let ((flag (list false))
        (val n))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! flag)
                 (the-semaphore 'acquire) ;retry
                 (if (> val 0)
                     (begin
                       (set! val (- val 1))
                       (clear! flag))
                     (begin
                       (clear! flag)
                       (the-semaphore 'acquire))))) ;retry
            ((eq? m 'release)
             (if (test-and-set! flag)
                 (the-semaphore 'release) ;retry
                 (if (< val n)
                     (begin
                       (set! val (+ val 1))
                       (clear! flag))
                     (clear! flag))))
            ((eq? m 'value)
             val)))
    the-semaphore))

(define s-m (make-semaphore-m 3))
(define s-t (make-semaphore-t 3))

