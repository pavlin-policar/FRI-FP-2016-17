#lang racket

#|
Gotchas:
Everything that is not explicitly #f is evaluated to true
|#

(define (potenca x n)
  (if (< n 1)
      1
      (* x (potenca x (- n 1)))))

#|
Notice how pristej1 is not in parens like potenca is. This is a shortcut so we don't have to writes
lambda 5mio times.
|#
(define pristej1
  (lambda (x)
    (+ x 1)))

; Some builtins take multiple args
(define sum1 (+ 1 2 3 4))
(define eq1 (= 1 2 3 4))

; What does currying look like here? Clearly, this is shit syntax. Can apparently be solved with
; macros...
(define ((add x) y)
  (+ x y))

; Sum the elements in the list
(define (sum ls)
  (if (null? ls)
      0
      (+ (car ls) (sum (cdr ls)))))

; Count elements in list
(define (count ls)
  (if (null? ls)
      0
      (+ 1 (count (cdr ls)))))

; Remove the first occurence of an element in the list
(define (removeFirst x ls)
  (if (null? ls)
      null
      (if (= x (car ls))
          (cdr ls)
          (cons (car ls) (removeFirst x (cdr ls))))))

; Remove every occurence of an element in a list
(define (removeEvery x ls)
  (if (null? ls)
      null
      (if (= x (car ls))
          (removeEvery x (cdr ls))
          (cons (car ls) (removeEvery x (cdr ls))))))

; Remove every occurence of an element in a list using cond
(define (removeEvery2 x ls)
  (cond [(null? ls) null]
        [(= x (car ls)) (removeEvery2 x (cdr ls))]
        [#t (cons (car ls) (removeEvery2 x (cdr ls)))]))

; Get the nth element in a list
(define (getNth n ls)
  (if (= n 0)
      (car ls)
      (getNth (- n 1) (cdr ls))))

; Get all but the nth element in a list
(define (allButNth n ls)
  (cond [(null? ls) null]
        [(= n 0) (cdr ls)]
        [#t (cons (car ls) (allButNth (- n 1) (cdr ls)))]))

; Flatten a list of lists
(define (flattenList ls)
  (cond [(null? ls) null]
        [(list? (car ls)) (append (flattenList (car ls)) (flattenList (cdr ls)))]
        [#t (cons (car ls) (flattenList (cdr ls)))]))

; Our own map
(define (map1 f ls)
  (if (null? ls)
      null
      (cons (f (car ls)) (map1 f (cdr ls)))))

; Our own filter
(define (filter1 f ls)
  (if (null? ls)
      null
      (if (f (car ls))
          (cons (car ls) (filter1 f (cdr ls)))
          (filter1 f (cdr ls)))))

; Our own foldl
(define (foldl1 f init ls)
  (if (null? ls)
      init
      (foldl f (f (car ls) init) (cdr ls))))

