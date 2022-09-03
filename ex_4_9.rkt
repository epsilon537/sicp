#lang racket

;Exercise 4.9: Many languages support a variety of iteration constructs, such as do, for, while, and until. In Scheme, iterative processes can be expressed in terms of ordinary ;procedure calls, so special iteration constructs provide no essential gain in computational power. On the other hand, such constructs are often convenient. Design some iteration ;constructs, give examples of their use, and show how to implement them as derived expressions.

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (lambda? exp) 
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (let? exp) 
  (tagged-list? exp 'let))
(define (named-let? exp)
  (and (let? exp) (= (length exp) 4)))
(define (named-let-name exp)
  (cadr exp))   
(define (let-clauses exp)
  (if (named-let? exp) (caddr exp) (cadr exp)))
(define (let-body exp)
  (if (named-let? exp) (cdddr exp) (cddr exp)))
(define (make-let-body exp)
  (if (named-let? exp)
      (let ((sig (cons (named-let-name exp)
                       (map let-var (let-clauses exp)))))
        (list (cons 'define (cons sig (let-body exp))) sig))
      (let-body exp)))
(define (let-var clause) (car clause))
(define (let-exp clause) (cadr clause))

(define (let->combination exp)
      (cons (make-lambda (map let-var (let-clauses exp))
                         (make-let-body exp))
            (map let-exp (let-clauses exp))))

(define (make-named-let name clauses body)
  (list 'let name clauses body))

(define (make-let clauses body)
  (list 'let clauses body))

;(while pred body)
(define (while? exp) 
  (tagged-list? exp 'while))

(define (while-pred exp)
  (cadr exp))

(define (while-body exp)
  (cddr exp))

(define (while->combination exp)
  (let->combination (make-named-let 'while-fun '()
                                    (list 'if (while-pred exp)
                                          (list (make-lambda '() (append (while-body exp) (list '(while-fun)))))
                                          'false))))

(while->combination '(while (> x 0)
                            (set! x (- x 1))
                            (display x)
                            (newline)))
                       
;(for var lst body)
;(let 'for-fun ((var lst)) (if (empty? var) false (begin body (for-fun (cdr var)))
(define (for? exp)
  (tagged-list? exp 'for))
(define (for-var exp)
  (cadr exp))
(define (for-list exp)
  (caddr exp))
(define (for-body exp)
  (cdddr exp))

(define (for->combination exp)
  (let->combination (make-named-let 'for-fun (list (list 'for-list (for-list exp)))
                                    (list 'if (list 'empty? 'for-list)
                                          'false
                                          (make-let (list (list (for-var exp) '(car for-list))) 
                                                    (list (make-lambda '() (append (for-body exp) (list '(for-fun (cdr for-list)))))))))))

(for->combination '(for x (list 1 2 3 4) (display x) (newline)))

  
