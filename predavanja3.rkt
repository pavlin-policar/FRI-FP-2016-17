#lang racket

; Memoization
(define fib
  (letrec ([resitve null]
           [pomozna (lambda (x)
                      (let ([ans (assoc x resitve)])
                        (if ans
                            (cdr ans)
                            (let ([nova  (cond [(= x 1) 1]
                                               [(= x 2) 1]
                                               [#t (+ (pomozna (- x 1))
                                                      (pomozna (- x 2)))])])
                              (begin
                                (set! resitve (cons (cons x nova) resitve))
                                ; (displayln resitve)
                                nova)))))])
    pomozna))

; Macro system
(define-syntax my-if
  (syntax-rules (then else)
    [(my-if e1 then e2 else e3)
     (if e1 e2 e3)]))

(define-syntax if3
  (syntax-rules (then elif else)
    [(if3 e1 then e2 elif e3 then e4 else e5)
         (if e1 e2 (if e3 e4 e5))]))
