#lang racket

(require rnrs/mutable-pairs-6) ;for set-car! and set-cdr!
(require compatibility/mlist) ;for mlist

(define cons mcons)
(define car mcar)
(define cdr mcdr)
(define (cddr m) (cdr (cdr m)))
(define (cadr m) (car (cdr m)))
(define list mlist)
(define pair? mpair?)
(define length mlength)

(define (filter pred xs)
  (if (null? xs)
      '()
      (if (pred (car xs))
          (cons (car xs) (filter pred (cdr xs)))
          (filter pred (cdr xs)))))

;Exercise 3.23: A deque (“double-ended queue”) is a sequence in which items can be inserted and deleted at either the front or the rear. Operations on deques are the constructor
;make-deque, the predicate empty-deque?, selectors front-deque and rear-deque, and mutators front-insert-deque!, rear-insert-deque!, front-delete-deque!, rear-delete-deque!. Show how to
;represent deques using pairs, and give implementations of the operations.151 All operations should be accomplished in Θ ( 1 ) steps.

(define (prev-ptr item) (cadr item))
(define (next-ptr item) (cddr item))
(define (value item) (car item))
(define (set-prev-ptr! item ptr)
  (set-car! (cdr item) ptr))
(define (set-next-ptr! item ptr)
  (set-cdr! (cdr item) ptr))
(define (set-value! item value)
  (set-car! item value))

(define (make-item value) (cons value (cons '() '())))

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) 
  (set-car! deque item))
(define (set-rear-ptr! deque item) 
  (set-cdr! deque item))

(define (make-deque)
  (cons '() '()))

(define (empty-deque? deque) (or (null? (front-ptr deque))  (null? (rear-ptr deque))))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (value (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (value (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-item (make-item item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-item)
           (set-rear-ptr! deque new-item)
           deque)
          (else (set-next-ptr! new-item (front-ptr deque))
                (set-prev-ptr! (front-ptr deque) new-item)
                (set-front-ptr! deque new-item)
                deque))))

(define (rear-insert-deque! deque item)
  (let ((new-item (make-item item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-item)
           (set-rear-ptr! deque new-item)
           deque)
          (else (set-prev-ptr! new-item (rear-ptr deque))
                (set-next-ptr! (rear-ptr deque) new-item)
                (set-rear-ptr! deque new-item)
                deque))))


(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT DELETE! called with an empty deque" deque))
        (else (set-front-ptr! 
               deque 
               (next-ptr (front-ptr deque)))
              (if (not (null? (front-ptr deque)))
                  (set-prev-ptr! (front-ptr deque) '())
                  '())
              deque)))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR DELETE! called with an empty deque" deque))
        (else (set-rear-ptr! 
               deque 
               (prev-ptr (rear-ptr deque)))
              (if (not (null? (rear-ptr deque)))
                  (set-next-ptr! (rear-ptr deque) '())
                  '())
              deque)))

(define q1 (make-deque))
(empty-deque? q1)
;(front-deque q1)
;(rear-deque q1)
(front-insert-deque! q1 'a)
(front-deque q1)
(rear-deque q1)
(front-insert-deque! q1 'b)
(front-deque q1)
(rear-deque q1)
(rear-insert-deque! q1 'c)
(front-deque q1)
(rear-deque q1)
(rear-insert-deque! q1 'd)
(front-deque q1)
(rear-deque q1)
(front-delete-deque! q1)
(front-deque q1)
(rear-deque q1)
(rear-delete-deque! q1)
(front-deque q1)
(rear-deque q1)
(front-delete-deque! q1)
(front-deque q1)
(rear-deque q1)
(rear-delete-deque! q1)
(empty-deque? q1)
(front-insert-deque! q1 'a)
(front-deque q1)
(rear-deque q1)
(rear-insert-deque! q1 'b)
(front-deque q1)
(rear-deque q1)
(empty-deque? q1)