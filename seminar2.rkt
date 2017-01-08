#lang racket

; Type declarations
(struct int (n) #:transparent)
(struct true () #:transparent)
(struct false () #:transparent)
(struct frac (e1 e2) #:transparent)
(struct ::  (e1 e2) #:transparent)
(struct empty () #:transparent)

; Flow control
(struct if-then-else (cnd e1 e2))
(struct is-int (e))
(struct is-bool (e))
(struct is-frac (e))
(struct is-list (e))

; Arithmetic
(struct add (e1 e2))
(struct mul (e1 e2))
(struct gt (e1 e2))

; Logical operations
(struct both (e1 e2))
(struct any (e1 e2))
(struct ! (e))

; List operations
(struct hd (x))
(struct tl (x))
(struct is-empty (x))
(struct @ (a b))

; Fraction operations
(struct numerator (e))
(struct denominator (e))


; Calculate the GCD of two numbers using the Euclidean method
(define (gcd x y) (if (= y 0) x (gcd y (modulo x y))))

; Calculate the LCM of two numbers
(define (lcm x y) (/ (* x y) (gcd x y)))

; Convert an int to a fraction by dividing it by one
(define (to-frac x) (frac x (int 1)))

(define (mi e env)
  (cond [(int? e) e]
        [(true? e) e]
        [(false? e) e]
        [(::? e) (:: (mi (::-e1 e) env) (mi (::-e2 e) env))]
        [(empty? e) e]
        ; Convert fractions to their simplest form
        [(frac? e)
         (letrec ([v1 (mi (frac-e1 e) env)]
                  [v2 (mi (frac-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (let ([factor (gcd (int-n v1) (int-n v2))])
                 (frac (int (/ (int-n v1) factor)) (int (/ (int-n v2) factor))))
               (error "The elements of the fraction are not ints")))]
        ; Flow control
        [(if-then-else? e)
         (let ([vcond (mi (if-then-else-cnd e) env)])
           (if (true? vcond)
               (mi (if-then-else-e1 e) env)
               (mi (if-then-else-e2 e) env)))]
        ; Type checking
        [(is-int? e) (if (int? (mi (is-int-e e) env)) (true) (false))]
        [(is-bool? e) (let ([v (mi (is-bool-e e) env)])
                        (if (or (true? v) (false? v)) (true) (false)))]
        [(is-frac? e) (if (frac? (mi (is-frac-e e) env)) (true) (false))]
        [(is-list? e) (let ([v (mi (is-list-e e) env)])
                        (if (or (::? v) (empty? v)) (true) (false)))]
        ; Arithmetic operations
        [(add? e)
         (let ([v1 (mi (add-e1 e) env)]
               [v2 (mi (add-e2 e) env)])
           (cond [(and (int? v1) (int? v2)) (int (+ (int-n v1) (int-n v2)))]
                 [(int? v1) (mi (add (to-frac v1) v2) env)]
                 [(int? v2) (mi (add v1 (to-frac v2)) env)]
                 [(and (frac? v1) (frac? v2))
                  ; Extract all the numbers out for simpler access
                  (let ([x1 (int-n (frac-e1 v1))]
                        [y1 (int-n (frac-e2 v1))]
                        [x2 (int-n (frac-e1 v2))]
                        [y2 (int-n (frac-e2 v2))])
                    ; Since we're not using LCM multiplication, simply make sure to simplify
                    ; equation after adding together
                    (mi (frac (int (+ (* x1 y2) (* x2 y1))) (int (* y1 y2))) env))]
                 [#t (error "The elements cannot be added")]))]
        [(mul? e)
         (let ([v1 (mi (mul-e1 e) env)]
               [v2 (mi (mul-e2 e) env)])
           (cond [(and (int? v1) (int? v2)) (int (* (int-n v1) (int-n v2)))]
                 [(int? v1) (mi (mul (to-frac v1) v2) env)]
                 [(int? v2) (mi (mul v1 (to-frac v2)) env)]
                 [(and (frac? v1) (frac? v2))
                  (mi (frac (int (* (int-n (frac-e1 v1)) (int-n (frac-e1 v2))))
                            (int (* (int-n (frac-e2 v1)) (int-n (frac-e2 v2))))) env)]))]
        [(gt? e)
         (let ([v1 (mi (gt-e1 e) env)]
               [v2 (mi (gt-e2 e) env)])
           (cond [(and (int? v1) (int? v2)) (if (> (int-n v1) (int-n v2)) (true) (false))]
                 [(int? v1) (mi (gt (to-frac v1) v2) env)]
                 [(int? v2) (mi (gt v1 (to-frac v2)) env)]
                 [(and (frac? v1) (frac? v2))
                  ; We won't do any magic and just let racket compare the two real numbers
                  (if (> (/ (int-n (frac-e1 v1)) (int-n (frac-e2 v1)))
                         (/ (int-n (frac-e1 v2)) (int-n (frac-e2 v2)))) (true) (false))]))]
        ; Logcal operations
        [(both? e) (if (and (true? (mi (both-e1 e) env))
                            (true? (mi (both-e2 e) env))) (true) (false))]
        [(any? e) (if (or (true? (mi (any-e1 e) env))
                          (true? (mi (any-e2 e) env))) (true) (false))]
        [(!? e) (if (true? (mi (!-e e) env)) (false) (true))]
        ; List operations
        [(hd? e)
         (let ([ls (mi (hd-x e) env)])
           (if (::? ls) (::-e1 ls) (error "List is not valid")))]
        [(tl? e)
         (let ([ls (mi (tl-x e) env)])
           (if (::? ls) (::-e2 ls) (error "List is not valid")))]
        [(is-empty? e)
         (let ([ls (mi (is-empty-x e) env)])
           (if (empty? ls) (true) (false)))]
        [(@? e)
         (let ([ls (mi (@-a e) env)])
           (if (empty? ls)
               (mi (@-b e) env)
               (mi (:: (hd ls) (@ (tl ls) (@-b e))) env)))]
        ; Fraction operations
        [(numerator? e) (frac-e1 (mi (numerator-e e) env))]
        [(denominator? e) (frac-e2 (mi (denominator-e e) env))]
        [#t (error "Not implemented")]))


; Begin test section
(define (assert e) (if e (void) (error "Assertion error")))
(define (assert-eq e1 e2) (assert (equal? e1 e2)))
(define (assert-true e) (assert (true? e)))
(define (assert-false e) (assert (false? e)))
(define (assert-empty e) (assert (empty? e)))
(define (assert-not-empty e) (assert (not (empty? e))))
#||#
; Arithmetic operations
(assert-eq (int 8)
           (mi (add (int 3) (int 5)) (list)))
(assert-eq (frac (int 1) (int 1))
           (mi (add (frac (int 1) (int 2)) (frac (int 1) (int 2))) (list)))
(assert-eq (frac (int 7) (int 2))
           (mi (add (int 3) (frac (int 1) (int 2))) (list)))
(assert-eq (frac (int 7) (int 2))
           (mi (add (frac (int 1) (int 2)) (int 3)) (list)))

(assert-eq (int 15)
           (mi (mul (int 5) (int 3)) (list)))
(assert-eq (frac (int 3) (int 8))
           (mi (mul (frac (int 3) (int 4)) (frac (int 1) (int 2))) (list)))
(assert-eq (frac (int 3) (int 2))
           (mi (mul (int 3) (frac (int 1) (int 2))) (list)))
(assert-eq (frac (int 3) (int 2))
           (mi (mul (frac (int 1) (int 2)) (int 3)) (list)))

(assert-true (mi (gt (int 5) (int 3)) (list)))
(assert-false (mi (gt (int 3) (int 5)) (list)))
(assert-false (mi (gt (int 5) (int 5)) (list)))
(assert-true (mi (gt (frac (int 1) (int 2)) (frac (int 1) (int 4))) (list)))
(assert-false (mi (gt (frac (int 1) (int 4)) (frac (int 1) (int 2))) (list)))
(assert-true (mi (gt (int 1) (frac (int 1) (int 2))) (list)))
(assert-false (mi (gt (int 1) (frac (int 3) (int 2))) (list)))

; Logical operations
(assert-true (mi (both (gt (int 3) (int 2)) (gt (int 3) (int 2))) (list)))
(assert-false (mi (both (gt (int 3) (int 4)) (gt (int 3) (int 2))) (list)))
(assert-false (mi (both (gt (int 3) (int 2)) (gt (int 3) (int 4))) (list)))
(assert-false (mi (both (gt (int 3) (int 4)) (gt (int 3) (int 4))) (list)))

(assert-true (mi (any (gt (int 3) (int 2)) (gt (int 3) (int 2))) (list)))
(assert-true (mi (any (gt (int 3) (int 4)) (gt (int 3) (int 2))) (list)))
(assert-true (mi (any (gt (int 3) (int 2)) (gt (int 3) (int 4))) (list)))
(assert-false (mi (any (gt (int 3) (int 4)) (gt (int 3) (int 4))) (list)))

(assert-false (mi (! (gt (int 3) (int 2))) (list)))
(assert-true (mi (! (gt (int 3) (int 4))) (list)))

; List operations
(assert-eq (int 2)
           (mi (hd (:: (int 2) (:: (int 3) (int 4)))) (list)))

(assert-eq (:: (int 3) (int 4))
           (mi (tl (:: (int 2) (:: (int 3) (int 4)))) (list)))

(assert-false (mi (is-empty (:: (int 2) (:: (int 3) (int 4)))) (list)))
(assert-true (mi (is-empty (empty)) (list)))

(assert-eq (:: (int 2) (:: (int 3) (:: (int 4) (:: (int 5) (empty)))))
           (mi (@ (:: (int 2) (:: (int 3) (empty))) (:: (int 4) (:: (int 5) (empty)))) (list)))
(assert-empty (mi (@ (empty) (empty)) (list)))

; Fraction operations
(assert-eq (int 4) (mi (numerator (frac (add (int 5) (int 3)) (int 2))) (list)))
(assert-eq (int 1) (mi (denominator (frac (add (int 5) (int 3)) (int 2))) (list)))
#||#
