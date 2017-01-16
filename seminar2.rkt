#lang racket

; Type declarations
(struct int (n) #:transparent)
(struct true () #:transparent)
(struct false () #:transparent)
(struct frac (e1 e2) #:transparent)
(struct ::  (e1 e2) #:transparent)
(struct empty () #:transparent)

; Flow control
(struct if-then-else (cnd e1 e2) #:transparent)
(struct is-int (e) #:transparent)
(struct is-bool (e) #:transparent)
(struct is-frac (e) #:transparent)
(struct is-list (e) #:transparent)

; Arithmetic
(struct add (e1 e2) #:transparent)
(struct mul (e1 e2) #:transparent)
(struct gt (e1 e2) #:transparent)

; Logical operations
(struct both (e1 e2) #:transparent)
(struct any (e1 e2) #:transparent)
(struct ! (e) #:transparent)

; List operations
(struct hd (x) #:transparent)
(struct tl (x) #:transparent)
(struct is-empty (x) #:transparent)
(struct @ (a b) #:transparent)

; Fraction operations
(struct numerator (e) #:transparent)
(struct denominator (e) #:transparent)

; Variables
(struct var (s e1 e2) #:transparent)
(struct valof (s) #:transparent)

; Functions
(struct fun (name fargs body) #:transparent)
(struct proc (name body) #:transparent)
(struct envelope (env f) #:transparent)
(struct call (e args) #:transparent)

(struct sum (e1) #:transparent)

; Find all used variable names inside an expression
(define find-used
  (lambda (x)
    (cond [(valof? x) (list (valof-s x))]
          ; return null on leaf nodes
          [(or (int? x) (true? x) (false? x) (empty? x)) null]
          [(frac? x) (append (find-used (frac-e1 x)) (find-used (frac-e2 x)))]
          [(::? x) (append (find-used (::-e1 x)) (find-used (::-e2 x)))]
          [(if-then-else? x) (append (find-used (if-then-else-cnd x))
                                     (find-used (if-then-else-e1 x))
                                     (find-used (if-then-else-e2 x)))]
          [(is-int? x) (find-used (is-int-e x))]
          [(is-bool? x) (find-used (is-bool-e x))]
          [(is-frac? x) (find-used (is-frac-e x))]
          [(is-list? x) (find-used (is-list-e x))]
          [(add? x) (append (find-used (add-e1 x)) (find-used (add-e2 x)))]
          [(mul? x) (append (find-used (mul-e1 x)) (find-used (mul-e2 x)))]
          [(gt? x) (append (find-used (gt-e1 x)) (find-used (gt-e2 x)))]
          [(both? x) (append (find-used (both-e1 x)) (find-used (both-e2 x)))]
          [(any? x) (append (find-used (any-e1 x)) (find-used (any-e2 x)))]
          [(!? x) (find-used (!-e x))]
          [(hd? x) (find-used (hd-x x))]
          [(tl? x) (find-used (tl-x x))]
          [(is-empty? x) (find-used (is-empty-x x))]
          [(@? x) (append (find-used (@-a x)) (find-used (@-b x)))]
          [(numerator? x) (find-used (numerator-e x))]
          [(denominator? x) (find-used (denominator-e x))]
          [(var? x) (append (find-used (var-e1 x)) (find-used (var-e2 x)))]
          [(fun? x) (find-used (fun-body x))]
          [(proc? x) (find-used (proc-body x))]
          [(envelope? x) (find-used (envelope-f x))]
          [(call? x) (find-used (call-e x))]
          [#t null])))

(define (mi e env)
  (cond [(int? e) e]
        [(true? e) e]
        [(false? e) e]
        [(::? e)
         (let ([v1 (mi (::-e1 e) env)]
               [v2 (mi (::-e2 e) env)])
           ; Proper handling of strange list structures
           (cond [(and (empty? v1) (empty? v2)) (empty)]
                 [(empty? v1) (if (::? v2) v2 (:: v2 (empty)))]
                 [(empty? v2) (if (::? v1) v1 (:: v1 (empty)))]
                 [#t (:: v1 v2)]))]
        [(empty? e) e]
        ; Convert fractions to their simplest form
        [(frac? e)
         (letrec ([v1 (mi (frac-e1 e) env)]
                  [v2 (mi (frac-e2 e) env)]
                  ; Calculate the GCD of two numbers using the Euclidean method
                  [gcd (lambda (x y) (if (= y 0) x (gcd y (modulo x y))))])
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
        ; Variables with eager evaluation
        [(var? e) (mi (var-e2 e) (cons (cons (var-s e) (mi (var-e1 e) env)) env))]
        [(valof? e) (mi (cdr (assoc (valof-s e) env)) env)]
        ; Functions
        [(fun? e)
         (let* ([no-shadow (remove-duplicates env #:key (lambda (x) (car x)))] ; shadowed vars
                [no-params (remove* (cons (fun-name e) (fun-fargs e)) ; fargs and fname
                                    no-shadow
                                    (lambda (x y) (equal? x (car y))))]
                [no-unused
                 (let ([used (find-used e)])
                   (filter (lambda (x) (member (car x) used)) no-params))])
           (envelope env e))]
        [(proc? e) e]
        [(envelope? e) e]
        [(call? e)
         (let ([f (mi (call-e e) env)])
           (cond
             ; Dynamic scoping
             ; Don't forget to add the procedure itself into the env unless lambda expression
             [(proc? f)
              (let ([fenv (cons (if (eq? (proc-name f) "") null (cons (proc-name f) f)) env)])
                (mi (proc-body f) fenv))]
             ; Lexicographical scoping
             [(envelope? f)
              (letrec ([zip (lambda (l1 l2) (map cons l1 l2))]
                       [locals (append
                                (map (lambda (x) (cons (car x) (mi (cdr x) env)))
                                     (zip (fun-fargs (envelope-f f)) (call-args e)))
                                ; Do not add the function to environment if lambda
                                ; No need to evaluate the function envelope again
                                (if (eq? (fun-name (envelope-f f)) "")
                                    null
                                    (list (cons (fun-name (envelope-f f)) f))))]
                       ; Include locals with the outer environment
                       [fenv (append locals (envelope-env f))])
                (mi (fun-body (envelope-f f)) fenv))]
             ; In the case that the variable gets shadowed and is not an actual function, simply
             ; return that value
             [#t (mi f env)]))]
        [(sum? e)
         (let ([add2 (fun "add2" (list "x" "y") (add (valof "x") (valof "y")))])
           (mi (call foldr (list add2 (int 0) (sum-e1 e))) env))]
        [#t e]))

; Macros
(define (to-frac x) (frac x (int 1)))
(define (inv e) (var "v" e (frac (denominator (valof "v")) (numerator (valof "v")))))
(define (~ e) (mul e (int -1)))
(define (lt e1 e2) (gt e2 e1))
(define (same e1 e2)
  (var "v1" e1
       (var "v2" e2
            (! (any (gt (valof "v1") (valof "v2")) (lt (valof "v1") (valof "v2")))))))
(define (sum-macro e1)
  (var "f" (fun "f" (list "ls")
                (if-then-else
                 (is-empty (valof "ls"))
                 (int 0)
                 (add (hd (valof "ls")) (call (valof "f") (list (tl (valof "ls")))))))
       (call (valof "f") (list e1))))

#| SML Like types

Definition
----------
With this syntax, we can define types in the following manner:

(datatype Color can be
          Red or
          Green or
          Blue)

This will define four different types: Color, Red, Green and Blue.

Usage
-----
TODO

|# 
(define-syntax datatype
  (syntax-rules (can be)
    [(datatype type can be rest ...)
     (begin
       ; Create the base type struct
       (struct type () #:transparent)
       ; Create all the instances of the base type
       (make-types type rest ...))]))

(define-syntax make-types
  (syntax-rules (or)
    ; Match the final instance
    [(make-types type instance) (struct instance type () #:transparent)]
    ; Make sure we can match multiple instances of type
    [(make-types type instance or rest ...)
     (begin
       (struct instance type () #:transparent)
       (make-types type rest ...))]))

(datatype Color can be
          Red or
          Green or
          Blue)

;-----------------------------------------------------------------------------------------------------
; BEGIN TEST SECTION
;-----------------------------------------------------------------------------------------------------
#||#

; Define assertion helpers
(define (assert e) (unless e (error "Assertion error")))
(define (assert-eq e1 e2) (assert (equal? e1 e2)))
(define (assert-true e) (assert (true? e)))
(define (assert-false e) (assert (false? e)))
(define (assert-empty e) (assert (empty? e)))
(define (assert-not-empty e) (assert (not (empty? e))))

; Arithmetic operations
(assert-eq (int 8) (mi (add (int 3) (int 5)) null))
(assert-eq (frac (int 1) (int 1)) (mi (add (frac (int 1) (int 2)) (frac (int 1) (int 2))) null))
(assert-eq (frac (int 7) (int 2)) (mi (add (int 3) (frac (int 1) (int 2))) null))
(assert-eq (frac (int 7) (int 2)) (mi (add (frac (int 1) (int 2)) (int 3)) null))

(assert-eq (int 15) (mi (mul (int 5) (int 3)) null))
(assert-eq (frac (int 3) (int 8)) (mi (mul (frac (int 3) (int 4)) (frac (int 1) (int 2))) null))
(assert-eq (frac (int 3) (int 2)) (mi (mul (int 3) (frac (int 1) (int 2))) null))
(assert-eq (frac (int 3) (int 2)) (mi (mul (frac (int 1) (int 2)) (int 3)) null))

(assert-true (mi (gt (int 5) (int 3)) null))
(assert-false (mi (gt (int 3) (int 5)) null))
(assert-false (mi (gt (int 5) (int 5)) null))
(assert-true (mi (gt (frac (int 1) (int 2)) (frac (int 1) (int 4))) null))
(assert-false (mi (gt (frac (int 1) (int 4)) (frac (int 1) (int 2))) null))
(assert-true (mi (gt (int 1) (frac (int 1) (int 2))) null))
(assert-false (mi (gt (int 1) (frac (int 3) (int 2))) null))

; Logical operations
(assert-true (mi (both (gt (int 3) (int 2)) (gt (int 3) (int 2))) null))
(assert-false (mi (both (gt (int 3) (int 4)) (gt (int 3) (int 2))) null))
(assert-false (mi (both (gt (int 3) (int 2)) (gt (int 3) (int 4))) null))
(assert-false (mi (both (gt (int 3) (int 4)) (gt (int 3) (int 4))) null))

(assert-true (mi (any (gt (int 3) (int 2)) (gt (int 3) (int 2))) null))
(assert-true (mi (any (gt (int 3) (int 4)) (gt (int 3) (int 2))) null))
(assert-true (mi (any (gt (int 3) (int 2)) (gt (int 3) (int 4))) null))
(assert-false (mi (any (gt (int 3) (int 4)) (gt (int 3) (int 4))) null))

(assert-false (mi (! (gt (int 3) (int 2))) null))
(assert-true (mi (! (gt (int 3) (int 4))) null))

; List operations
(assert-eq (int 2) (mi (hd (:: (int 2) (:: (int 3) (int 4)))) null))

(assert-eq (:: (int 3) (int 4)) (mi (tl (:: (int 2) (:: (int 3) (int 4)))) null))

(assert-false (mi (is-empty (:: (int 2) (:: (int 3) (int 4)))) null))
(assert-true (mi (is-empty (empty)) null))

(assert-eq (:: (int 2) (:: (int 3) (:: (int 4) (:: (int 5) (empty)))))
           (mi (@ (:: (int 2) (:: (int 3) (empty))) (:: (int 4) (:: (int 5) (empty)))) null))
(assert-eq (:: (int 2) (:: (int 3) (empty)))
           (mi (@ (:: (int 2) (:: (int 3) (empty))) (empty)) null))
(assert-eq (:: (int 2) (:: (int 3) (empty)))
           (mi (@ (empty) (:: (int 2) (:: (int 3) (empty)))) null))
(assert-empty (mi (@ (empty) (empty)) null))

; Fraction operations
(assert-eq (int 4) (mi (numerator (frac (add (int 5) (int 3)) (int 2))) null))
(assert-eq (int 1) (mi (denominator (frac (add (int 5) (int 3)) (int 2))) null))

; Variables
(assert-eq (int 4) (mi (var "_" (true) (int 4)) null))
(assert-eq (int 4) (mi (valof "a") (list (cons "a" (int 4)))))
(assert-eq (int 4) (mi (var "a" (int 3) (var "b" (int 1) (add (valof "a") (valof "b")))) '()))
(assert-eq (int 2) (mi (var "a" (int 2) (call (fun "x" null (valof "a")) null)) null))
; test variable shadowing
(assert-eq (int 1) (mi (var "a" (int 2) (var "a" (int 1) (valof "a"))) null))

; Functions
; dynamically scoped functions
(assert-eq (int 2) (mi (call (proc "f" (int 2)) null) null))
(assert-eq (int 2) (mi (var "n" (int 1) (call (proc "foo" (mul (int 2) (valof "n"))) null)) null))

(define add-to
  (proc "add-to"
        (if-then-else
         (lt (valof "i") (int 3))
         (var "i" (add (valof "i") (int 1)) (call (valof "add-to") null))
         (valof "i"))))
(assert-eq (int 3) (mi (var "i" (int 1) (call add-to null)) null))

; lexicographicaly scope functions
(define add3 (fun "add3" (list "a" "b" "c") (add (valof "a") (add (valof "b") (valof "c")))))
(assert-eq (int 6) (mi (call add3 (list (int 1) (int 2) (int 3))) null))
; recursive power
(define power
  (fun "power" (list "a" "x")
       (if-then-else
        (! (gt (valof "x") (int 1)))
        (valof "a")
        (mul (valof "a")
             (call (valof "power") (list (valof "a") (add (valof "x") (int -1))))))))
(assert-eq (int 8) (mi (call power (list (int 2) (int 3))) null))
; sum of list
(define sum-of-list
  (fun "sum-of-list" (list "ls")
       (if-then-else
        (is-empty (tl (valof "ls")))
        (hd (valof "ls"))
        (add (hd (valof "ls")) (call (valof "sum-of-list") (list (tl (valof "ls"))))))))
(assert-eq (int 2) (mi (call sum-of-list (list (:: (int 2) (empty)))) null))
(assert-eq (int 7) (mi (call sum-of-list (list (:: (int 2) (:: (int 3) (:: (int 2) (empty)))))) null))

; shadow function name
(assert-eq (int 2) (mi (call (fun "f" null (var "f" (int 2) (valof "f"))) null) null))
(assert-eq (int 2) (mi (call (fun "f" null (var "f" (int 2) (call (valof "f") null))) null) null))

; clojure optimization
; check shadowed variables
;(assert-eq
; (list (cons "a" (int 2)) (cons "b" (true)))
; (envelope-env (mi (fun "f" null (add (valof "a") (valof "b")))
;                   (list (cons "a" (int 2)) (cons "a" (int 3)) (cons "b" (true))))))

; check params removed from env
;(assert-eq
; (list (cons "b" (int 2)))
; (envelope-env (mi (fun "f" (list "a" "c") (add (valof "a") (add (valof "b") (valof "c"))))
;                   (list (cons "a" (int 1)) (cons "b" (int 2)) (cons "c" (int 3))))))

; check function name removed from env
;(assert-eq
; (list (cons "a" (int 1)) (cons "c" (int 3)))
; (envelope-env (mi (fun "b" null (add (valof "a") (valof "c")))
;                   (list (cons "a" (int 1)) (cons "b" (int 2)) (cons "c" (int 3))))))

; find-used
;(assert-eq (list "a" "b") (find-used (:: (int 3) (:: (valof "a") (:: (valof "b") (empty))))))
;(assert-eq (list "a" "b")
;           (find-used (mi (fun "f" null (add (frac (valof "a") (int 2))
;                                             (frac (valof "b") (int 2)))) null)))

; check unused removed from env
;(assert-eq
; (list (cons "a" (int 1)))
; (envelope-env (mi (fun "f" null (valof "a"))
;                   (list (cons "a" (int 1)) (cons "b" (int 2)) (cons "c" (int 3))))))


;(assert-eq
; (list (cons "a" (int 1)) (cons "b" (int 2)))
; (envelope-env (mi (fun "f" null (add (valof "a") (valof "b")))
;                   (list (cons "a" (int 1)) (cons "b" (int 2)) (cons "c" (int 3)) (cons "d" (int 4))))))

; quicksort implementation
(define lte-list
  (fun "lte-list" (list "ls" "x")
       (if-then-else
        (is-empty (valof "ls"))
        (empty)
        (if-then-else
         (any (lt (hd (valof "ls")) (valof "x")) (same (hd (valof "ls")) (valof "x")))
         (:: (hd (valof "ls")) (call (valof "lte-list") (list (tl (valof "ls")) (valof "x"))))
         (call (valof "lte-list") (list (tl (valof "ls")) (valof "x")))))))
(define gt-list
  (fun "gt-list" (list "ls" "x")
       (if-then-else
        (is-empty (valof "ls"))
        (empty)
        (if-then-else
         (gt (hd (valof "ls")) (valof "x"))
         (:: (hd (valof "ls")) (call (valof "gt-list") (list (tl (valof "ls")) (valof "x"))))
         (call (valof "gt-list") (list (tl (valof "ls")) (valof "x")))))))
(define quicksort
  (fun "qs" (list "xs")
       (var "lte" lte-list
            (var "gt" gt-list
                 (if-then-else
                  (is-empty (valof "xs"))
                  (empty)
                  (@ (call (valof "qs") (list (call (valof "lte") (list (tl (valof "xs")) (hd (valof "xs"))))))
                     (:: (hd (valof "xs"))
                         (call (valof "qs") (list (call (valof "gt")
                                                        (list (tl (valof "xs")) (hd (valof "xs")))))))))))))
; test qs
(assert-eq
 (:: (int 1) (:: (int 2) (:: (int 3) (:: (int 4) (:: (int 5) (empty))))))
 (mi (call quicksort
           (list (:: (int 3) (:: (int 1) (:: (int 2) (:: (int 5) (:: (int 4) (empty))))))))
     null))

; map
(define map2
  (fun "map" (list "f" "ls")
       (if-then-else
        (is-empty (valof "ls"))
        (empty)
        (:: (call (valof "f") (list (hd (valof "ls"))))
            (call (valof "map") (list (valof "f") (tl (valof "ls"))))))))
(assert-eq
 (:: (int 2) (:: (int 3) (:: (int 4) (empty))))
 (mi (call map2 (list (fun "" (list "x") (add (valof "x") (int 1)))
           (:: (int 1) (:: (int 2) (:: (int 3) (empty)))))) null))

; reduce
(define foldr
  (fun "foldr" (list "f" "init" "ls")
       (if-then-else
        (is-empty (valof "ls"))
        (valof "init")
        (call (valof "foldr")
              (list (valof "f")
                    (call (valof "f") (list (hd (valof "ls")) (valof "init")))
                    (tl (valof "ls")))))))
(define add2 (fun "add2" (list "x" "y") (add (valof "x") (valof "y"))))
(assert-eq
 (int 6)
 (mi (call foldr (list add2 (int 0) (:: (int 1) (:: (int 2) (:: (int 3) (empty)))))) null))

; filter
(define filter2
  (fun "filter" (list "f" "ls")
       (if-then-else
        (is-empty (valof "ls"))
        (empty)
        (if-then-else
         (call (valof "f") (list (hd (valof "ls"))))
         (:: (hd (valof "ls")) (call (valof "filter") (list (valof "f") (tl (valof "ls")))))
         (call (valof "filter") (list (valof "f") (tl (valof "ls"))))))))
(assert-eq
 (:: (true) (:: (false) (empty)))
 (mi (call filter2 (list (fun "" (list "x") (is-bool (valof "x")))
                         (:: (int 1) (:: (true) (:: (int 3) (:: (false) (:: (int 4) (empty))))))))
     null))


; Macros
(assert-eq (frac (int 2) (int 1)) (mi (to-frac (int 2)) null))

(assert-eq (frac (int 4) (int 3)) (mi (inv (frac (add (int 1) (int 2)) (mul (int 2) (int 2)))) null))

(assert-eq (int -5) (mi (~ (int 5)) null))
(assert-eq (int 5) (mi (~ (int -5)) null))

(assert-false (mi (lt (int 5) (int 3)) null))
(assert-true (mi (lt (int 3) (int 5)) null))
(assert-false (mi (lt (int 5) (int 5)) null))
(assert-false (mi (lt (frac (int 1) (int 2)) (frac (int 1) (int 4))) null))
(assert-true (mi (lt (frac (int 1) (int 4)) (frac (int 1) (int 2))) null))
(assert-false (mi (lt (int 1) (frac (int 1) (int 2))) null))
(assert-true (mi (lt (int 1) (frac (int 3) (int 2))) null))

(assert-true (mi (same (int 5) (int 5)) null))
(assert-false (mi (same (int 4) (int 5)) null))
(assert-false (mi (same (int 5) (int 4)) null))
(assert-true (mi (same (frac (int 1) (int 2)) (frac (int 1) (int 2))) null))
(assert-false (mi (same (frac (int 1) (int 3)) (frac (int 1) (int 2))) null))
(assert-false (mi (same (frac (int 1) (int 2)) (frac (int 1) (int 3))) null))

; Additional exercises
; add fractions
(assert-eq
 (frac (int 7) (int 2))
 (mi (add (frac (int 1) (int 2)) (int 3)) null))

; sum
(assert-eq
 (int 4)
 (mi (var "x" (int 1) (sum (:: (int 1) (:: (add (int 1) (valof "x")) (:: (int 1) (empty)))))) null))

; sum-macro
(assert-eq
 (int 4)
 (mi (var "l" (:: (int 1) (:: (add (int 1) (int 1)) (:: (int 1) (empty)))) (sum-macro (valof "l"))) null))

#||#
