#lang racket

(struct int (n) #:transparent)
(struct true () #:transparent)
(struct false () #:transparent)

; Arithmetic
(struct add (e1 e2) #:transparent)
(struct gt (e1 e2) #:transparent)

; Logical operations
(struct both (e1 e2) #:transparent)
(struct ! (e) #:transparent)

; Typechecking
(struct is-int (e) #:transparent)

; Flow control
(struct if-then-else (cond e1 e2) #:transparent)

(define (mi e)
  (cond [(int? e) e]
        [(true? e) e]
        [(false? e) e]
        ; Add
        [(add? e)
         (let ([v1 (mi (add-e1 e))]
               [v2 (mi (add-e2 e))])
           (if (and (int? v1) (int? v2))
               (int (+ (int-n v1) (int-n v2)))
               (error "Operands are not ints")))]
        ; Greater than
        [(gt? e)
         (let ([v1 (mi (gt-e1 e))]
               [v2 (mi (gt-e2 e))])
           (if (and (int? v1) (int? v2))
               (if (> (int-n v1) (int-n v2)) (true) (false))
               (error "Operands are not ints")))]
        ; Logical AND
        [(both? e)
         (let ([v1 (mi (both-e1 e))]
               [v2 (mi (both-e2 e))])
           (cond [(and (true? v1) (true? v2)) (true)]
                 [(and (or (true? v1) (false? v1)) (or (true? v2) (false? v2))) (false)]
                 [#t (error "Operands are not bools")]))]
        ; Logical NOT
        [(!? e)
         (let ([v (mi (!-e e))])
           (cond [(true? v) (false)]
                 [(false? v) (true)]
                 [#t (error "Value is not a bool")]))]
        ; Typecheck int
        [(is-int? e)
         (let ([v (mi (is-int-e e))])
           (if (int? v) (true) (false)))]
        ; Flow control if then else
        [(if-then-else? e)
         (let ([vcond (mi (if-then-else-cond e))])
           (if (true? vcond)
               (mi (if-then-else-e1 e))
               (mi (if-then-else-e2 e))))]))

; If then else syntax
(define-syntax ifte
  (syntax-rules (ifte then else)
    [(ifte cnd then left else right) (if-then-else cnd left right)]))

; Less than syntax
(define-syntax lt
  (syntax-rules (lt)
    [(lt left right) (gt right left)]))
