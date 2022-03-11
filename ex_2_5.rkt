#lang sicp

;Exercise 2.5: Show that we can represent pairs of nonnegative integers using only numbers and arithmetic operations if we represent the pair a and b as the integer that is the product 2
;a 3 b . Give the corresponding definitions of the procedures cons, car, and cdr.

;No power of 2 is multiple of 3:
;proved by mathematical induction.
;
;Clearly 2^0 = 1 is not a multiple of 3. This establishes our base case.
;
;Now, suppose that for some k ≥ 0 we know that 2^k is not a multiple of 3. We will show that this means 2^(k+1) is not a multiple of 3 either.
;
;If 2^k is not a multiple of 3, then it is either 1 greater than a multiple of 3, or else 2 greater than a multiple of 3.
;
;    In the first case, we have 2^k = 3m + 1 for some integer m. Then 2^(k+1) = 2⋅2^k = 2(3m + 1) = 3(2m) + 2, which is 2 greater than a multiple of 3. So 2^(k+1) is not a multiple of 3.
;
;    In the second case, we have 2^k = 3m + 2 for some integer m. Then 2^(k+1) = 2⋅2^k = 2(3m + 2) = 6m + 4 = 6m + 3 + 1 = 3(2m + 1) + 1, which is 1 greater than a multiple of 3. So again
;2k+1 ;is not a multiple of 3.
;
;Therefore, if 2^k is not a multiple of 3, then 2^(k+1) is not a multiple of 3 either. This completes the induction step of the proof.
;
;Since we know that 2^0 is not a multiple of 3, and we know that whenever 2^k is not a multiple of 3, 2^(k+1) is not a multiple of 3 either, this shows that no power of 2 is a multiple
;of 3.
;
;No power of 3 is multiple of 2. Similar proof by induction.
;
;=> If z = (2^a)(3^b), only the 3^b portion of that number is divisible by 3. We can repeatedly divide z by 3 b times. Only the 2^a portion is divisble by a. We can repeatedly divide
;z by 2 a times. This allows us to extra a and b from z

(define (pow b e)
  (define (pow-iter acc e)
    (if (= e 0)
        acc
        (pow-iter (* acc b) (dec e))))
  (pow-iter 1 e))

(define (cons a b)
  (* (pow 2 a) (pow 3 b)))

(define (car-cdr-iter d v n)
    (if (= 0 (remainder v d))
        (car-cdr-iter d (/ v d) (inc n))
        n))

(define (car z)
  (car-cdr-iter 2 z 0))

(define (cdr z)
  (car-cdr-iter 3 z 0))

(car (cons 23 109))
(cdr (cons 23 109))