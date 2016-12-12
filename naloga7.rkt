#lang racket

; Ones
(define ones (cons 1 (lambda () ones)))

; Natural numbers
(define naturals
  (letrec ([f (lambda (x) (cons (+ x 1) (lambda () (f (+ x 1)))))])
    (f 0)))

; Fibonacci numbers
(define fibs
  (letrec ([f (lambda (x1 x2)
            (let ([x (+ x1 x2)])
              (cons x1 (lambda () (f x2 x)))))])
    (f 1 1)))

; Take n elements from stream
(define (first n ls)
  (cond [(null? ls) null]
        [(= n 1) (cons (car ls) null)]
        [#t (cons (car ls) (first (- n 1) ((cdr ls))))]))

; Take a stream and return another stream with the squared numbers of that stream
(define (squares s)
  (letrec ([f (lambda (x)
             (let ([el (car x)]
                   [next ((cdr x))])
               (cons (* el el) (lambda () (f next)))))])
    (f s)))

; SML macro for lists, support (::), hd, tl, nil, null
(define-syntax sml
  (syntax-rules (nil null :: hd tl)
    [(sml nil) (list)]
    [(sml null e) (empty? e)]
    [(sml e :: nil) (list e)]
    [(sml e :: null) (list e)]
    [(sml e1 :: e2) (cons e1 e2)]
    [(sml hd e) (car e)]
    [(sml tl e) (cdr e)]))
