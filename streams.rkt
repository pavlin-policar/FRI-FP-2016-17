#lang racket

; Streams galore

(define take
  (lambda (n ls)
    (cond [(or (< n 1) (null? ls)) null]
          [#t (cons (car ls) (take (- n 1) ((cdr ls))))])))

(define take-while
  (lambda (f stream)
    (cond [(or (null? stream) (not (f (car stream)))) null]
          [#t (cons (car stream) (take-while f ((cdr stream))))])))

(define drop-while
  (lambda (f stream)
    (cond [(or (null? stream) (not (f (car stream)))) stream]
          [#t  (drop-while f ((cdr stream)))])))

; NON-PARAMETERIZED STREAMS

(define ones
  (cons 1 (lambda () ones)))

(define naturals
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (f 1)))

(define fibs
  (letrec ([f (lambda (x y) (cons x (lambda () (f y (+ x y)))))])
    (f 1 1)))

(define alternating
  (letrec ([f (lambda (x) (cons (* x -1) (lambda () (f (* x -1)))))])
    (f -1)))

; PARAMETERIZED STREAMS
; It may be cool to add params to streams

(define from
  (lambda (x)
    (cons x (lambda () (from (+ x 1))))))

(define powers-of
  (lambda (n)
    (letrec ([f (lambda (n x) (cons (expt n x) (lambda () (f n (+ x 1)))))])
      (f n 0))))

(define range
  (lambda (from step)
    (cons from (lambda () (range (+ from step) step)))))

; FUNCTIONS ON STREAMS

; Add arbitrary stream together
(define add
  (lambda streams
    (cons (foldl + 0 (map car streams)) (lambda () (apply add (map (lambda (x) ((cdr x))) streams))))))

; Multiply elements of stream together
(define multiply
  (lambda streams
    (cons (foldl * 1 (map car streams)) (lambda () (apply multiply (map (lambda (x) ((cdr x))) streams))))))

; Multiply an entire stream with a scalar
(define multiply-scalar
  (lambda (x stream)
    (cons (* x (car stream)) (lambda () (multiply-scalar x ((cdr stream)))))))

; Map over a stream
(define map-stream
  (lambda (f stream)
    (cons (f (car stream)) (lambda () (map-stream f ((cdr stream)))))))

; Fold arbitrary streams together
(define fold-streams
  (lambda (f init . streams)
    (cons (foldl f init (map car streams))
          (lambda () (apply fold-streams f init (map (lambda (x) ((cdr x))) streams))))))

; We can now redifine add using our new fold-streams function
(define add-with-fold
  (lambda streams
    (apply fold-streams + 0 streams)))

; FINITE STREAMS

(define range-till
  (lambda (start stop [step 1])
    (if (< start stop)
        (cons start (lambda () (range-till (+ start step) stop step)))
        null)))

; FUNCTION STREAMS

; This function returns a stream of funcitons that each access the n-th element of a list
(define indices
  (letrec ([get-nth (lambda (n ls) (if (< n 1) (car ls) (get-nth (- n 1) (cdr ls))))]
           [f (lambda (n) (cons (lambda (ls) (get-nth n ls))
                                (lambda () (f (+ n 1)))))])
    (f 0)))
