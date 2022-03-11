#lang sicp

;Exercise 1.37:
;
;    An infinite continued fraction is an expression of the form
;    f = N 1 D 1 + N 2 D 2 + N 3 D 3 + ... .
;    As an example, one can show that the infinite continued fraction expansion with the N i and the D i all equal to 1 produces 1 / φ , where φ is the golden ratio (described in 1.2.2).
;One way to approximate an infinite continued fraction is to truncate the expansion after a given number of terms. Such a truncation—a so-called finite continued fraction k-term finite
;continued fraction—has the form
;    N 1 D 1 + N 2 ⋱ + N k D k .
;    Suppose that n and d are procedures of one argument (the term index i ) that return the N i and D i of the terms of the continued fraction. Define a procedure cont-frac such that
;evaluating (cont-frac n d k) computes the value of the k -term finite continued fraction. Check your procedure by approximating 1 / φ using;
;
;    (cont-frac (lambda (i) 1.0)
;               (lambda (i) 1.0)
;               k)
;
;    for successive values of k. How large must you make k in order to get an approximation that is accurate to 4 decimal places?
;    If your cont-frac procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive
;process.

(define (cont-frac-rec n d k)
  (define (cont-frac-loop i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (cont-frac-loop (inc i))))))
  (cont-frac-loop 1))

(cont-frac-rec (lambda (i) 1.0)
               (lambda (i) 1.0)
               12) ; 12 iterations required for four accurate decimal places.

;For the iterative implementation, start with the k-th term (i=k) and walk back to the first term (i=1). 
(define (cont-frac-iter n d k)
  (define (cont-frac-loop i dacc)
    (if (= i 1)
        (/ (n i) dacc)
        (let ((q (/ (n i) dacc))
              (next-i (dec i)))
          (cont-frac-loop next-i (+ (d next-i) q)))))
  (cont-frac-loop k (d k)))

(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                12) ; 12 iterations required for four accurate decimal places.