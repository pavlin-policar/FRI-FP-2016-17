#lang racket

; Thunks
(define (my-delay thunk)
  (mcons #f thunk))

(define (my-force promise)
  (if (mcar promise)
      (mcdr promise)
      (begin (set-mcar! promise #t)
             (set-mcdr! promise ((mcdr promise)))
             (mcdr promise))))

(define (power x n)
  (cond [(= n 0) 1]
        [#t (* x (power x (- n 1)))]))


; (define x (my-delay (lambda () (+ 5 2))))
; Define the thunk
; (my-force x)
; Evaluate the thunk

; Streams

(define (take n ls)
  (cond [(= n 1) (cons (car ls) null)]
        [#t (cons (car ls) (take (- n 1) ((cdr ls))))]))

(define (takeWhile cond ls)
  (if (cond (car ls))
      (cons (car ls) (takeWhile cond ((cdr ls))))
      null))

(define (numTrue cond ls)
  (if (cond (car ls))
      (+ 1 (numTrue cond ((cdr ls))))
      0))

; Infinite steram of ones
(define ones (cons 1 (lambda () ones)))
; Natural numbers
(define natural
  (letrec ([f (lambda (x) (cons (+ x 1) (lambda () (f (+ x 1)))))])
    (f 0)))
; Alternating 1 and -1
(define alternating
  (letrec ([f (lambda (x) (cons (* x -1) (lambda () (f (* x -1)))))])
    (f -1)))
; Powers of two
(define powersTwo
  (letrec ([f (lambda (x) (cons (power 2 x) (lambda () (f (+ x 1)))))])
    (f 0)))
