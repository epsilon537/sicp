#lang racket

;Exercise 2.74: Insatiable Enterprises, Inc., is a highly decentralized conglomerate company consisting of a large number of independent divisions located all over the world. The
;company’s computer facilities have just been interconnected by means of a clever network-interfacing scheme that makes the entire network appear to any user to be a single computer.
;Insatiable’s president, in her first attempt to exploit the ability of the network to extract administrative information from division files, is dismayed to discover that, although all
;the division files have been implemented as data structures in Scheme, the particular data structure used varies from division to division. A meeting of division managers is hastily
;called to search for a strategy to integrate the files that will satisfy headquarters’ needs while preserving the existing autonomy of the divisions.
;
;Show how such a strategy can be implemented with data-directed programming. As an example, suppose that each division’s personnel records consist of a single file, which contains a set
;of records keyed on employees’ names. The structure of the set varies from division to division. Furthermore, each employee’s record is itself a set (structured differently from
;division to division) that contains information keyed under identifiers such as address and salary. In particular:
;
;    Implement for headquarters a get-record procedure that retrieves a specified employee’s record from a specified personnel file. The procedure should be applicable to any division’s
;file. Explain how the individual divisions’ files should be structured. In particular, what type information must be supplied?

;A:
;Individual divisions can structure their files any way they want but they have to provide an accessor function that allow to retrieve records.
;Each division's code should be placed in a package and the relevant function should be hooked into the dispatching table.
;The division should associate a unique division tag with the function.

;Implement for headquarters a get-salary procedure that returns the salary information from a given employee’s record from any division’s personnel file. How should the record be
;structured in order to make this operation work?

;A: Same answer as above.

;Implement for headquarters a find-employee-record procedure. This should search all the divisions’ files for the record of a given employee and return the record. Assume that this
;procedure takes as arguments an employee’s name and a list of all the divisions’ files.
;When Insatiable takes over a new company, what changes must be made in order to incorporate the new personnel information into the central system?

;A: No changes are necessary to the generic code. The new company has to provide its package function, with unique division tag, to hook into the dispatching table.

; Defining get and put in Racket:

(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: 
              TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: 
              CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types: 
             APPLY-GENERIC"
            (list op type-tags))))))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

;Generic functions:
(define (get-personnel-record div-name)
  (get 'get-personnel-record div-name))

(define (get-record emp_name personnel_rec)
  (apply-generic 'get-record (attach-tag 'name emp_name) personnel_rec))

(define (get-salary emp_rec)
  (apply-generic 'get-salary emp_rec))

(define (find-employee-record emp_name personnel_recs)
  (let ((unfiltered-recs (map (lambda (prec) (get-record emp_name prec)) personnel_recs)))
    (let ((filtered-recs (filter (lambda (erec) (not (null? erec))) unfiltered-recs)))
      filtered-recs)))

(define (install-div-a-package)
  ;In this package, the personnel record is a list of pairs. The car of the pair is the employee name, the cdr is the employee's record.
  ;The employee record is a list. The first element is the address, the second element is the salary
  (define personnel-record (list (cons 'Jim (list 'jims_address 2000)) (cons 'Joe (list 'joes_address 3000))))
  
  (define (get-record emp_name personnel_rec)
    (let ((matching-entries (filter (lambda (entry) (eq? (car entry) emp_name)) personnel_rec)))
      (let ((matching-recs (map cdr matching-entries)))
        (if (null? matching-recs)
            '()
            (car matching-recs)))))

  (define (get-salary emp_rec)
    (cadr emp_rec))

  (put 'get-personnel-record 'div-a (attach-tag 'div-a-personnel-rec personnel-record))
  (put 'get-record '(name div-a-personnel-rec) get-record)
  (put 'get-salary '(div-a-emp-rec) get-salary)
  'done)

(install-div-a-package)

(define (install-div-b-package)
  ;In this package, the personnel record is a list of lists. The inner list's first element is the employee name, the 2nd element is the employee's record.
  ;The employee record is a list. The first element is the salary, the second element is the address.
  (define personnel-record (list (list 'Jef (list 4000 'jefs_address)) (list 'Jake (list 5000 'jakes_address))))

   (define (get-record emp_name personnel_rec)
    (let ((matching-entries (filter (lambda (entry) (eq? (car entry) emp_name)) personnel_rec)))
      (let ((matching-recs (map cadr matching-entries)))
        (if (null? matching-recs)
            '()
            (car matching-recs)))))
  
  (define (get-salary emp_rec)
    (car emp_rec))

  (put 'get-personnel-record 'div-b (attach-tag 'div-b-personnel-rec personnel-record))
  (put 'get-record '(name div-b-personnel-rec) get-record)
  (put 'get-salary '(div-b-emp-rec) get-salary)

  'done)

(install-div-b-package)
