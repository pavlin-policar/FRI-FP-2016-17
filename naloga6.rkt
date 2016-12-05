#lang racket

; Power operator
(define (power x n)
  (cond [(= n 0) 1]
        [#t (* x (power x (- n 1)))]))

; Euclidean method for GCD
(define (gcd x y)
  (cond [(= y 0) x]
        [#t (gcd y (modulo x y))]))

; Take the n-th element from a stream
(define (take_nth n ls)
  (cond [(null? ls) null]
        [(= n 1) (car ls)]
        [#t (take_nth (- n 1) ((cdr ls)))]))

; Fibonacci stream
(define (fibonacci_stream)
  (letrec ([f (lambda (x1 x2)
                (let ([x (+ x1 x2)])
                  (cons x1 (lambda () (f x2 x)))))])
    (f 1 1)))

; Fibonacci sequence
(define (fib n) (take_nth n (fibonacci_stream)))

; Reverse a list
(define (reverse ls) (foldl (lambda (x a) (cons x a)) (list) ls))

; Remove occurences of x from list
(define (remove x ls) (filter (lambda (i) (not (= i x))) ls))

; Map
(define (map f ls)
  (if (null? ls)
      null
      (cons (f (car ls)) (map f (cdr ls)))))

; Filter
(define (filter f ls)
  (cond [(null? ls) null]
        [(f (car ls)) (cons (car ls) (filter f (cdr ls)))]
        [#t  (filter f (cdr ls))]))

; Zip
(define (zip l1 l2)
  (if (or (null? l1) (null? l2))
      null
      (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))

; Range
(define (range start stop step)
  (if (> start stop)
      null
      (cons start (range (+ start step) stop step))))

; Take n elements from stream
(define (take n ls)
  (cond [(null? ls) null]
        [(= n 1) (cons (car ls) null)]
        [#t (cons (car ls) (take (- n 1) ((cdr ls))))]))

; Range stream
(define (range_stream start step)
  (cons start (lambda () (range_stream (+ start step) step))))
