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
(struct numerator (e1))
(struct denominator (e1))


; Calculate the GCD of two numbers using the Euclidean method
(define (gcd x y) (if (= y 0) x (gcd y (modulo x y))))

; Calculate the LCM of two numbers
(define (lcm x y) (/ (* x y) (gcd x y)))

(define (mi e env)
  (cond [(int? e) e]
        [(true? e) e]
        [(false? e) e]
        [(::? e) e]
        [(empty? e) e]
        ; Convert fractions to their simplest form
        [(frac? e)
         (letrec ([v1 (mi (frac-e1 e) env)]
                  [v2 (mi (frac-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (let ([factor (gcd (int-n v1) (int-n v2))])
                 (frac (/ (int-n v1) factor) (/ (int-n v2) factor)))
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
        ; Arithmetic
        [(add? e)
         (let ([v1 (mi (add-e1 e) env)]
               [v2 (mi (add-e2 e) env)])
           (cond [(and (int? v1) (int? v2)) (int (+ (int-n v1) (int-n v2)))]
                 [#t (error "The elements cannot be added")]))]
        [#t (false)]))