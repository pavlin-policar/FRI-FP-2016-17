#lang racket

#|
 | JAIS language spec
 | ------------------
 | (konst 4)
 | (bool true)
 | (sestej e1 e2)
 | (negiraj e)
 | (ce-potem-sicer)
 |#

(struct konst (int) #:transparent)
(struct negiraj (e) #:transparent)
(struct sestej (e1 e2) #:transparent)
(struct bool (b) #:transparent)
(struct ce-potem-sicer (pogoj res nires) #:transparent)

; Konstrukti za okolje, use assoc with lists for named variables
(struct shrani (vrednost izraz) #:transparent)
(struct beri () #:transparent)

; Konstrukti za funkcije
(struct ovojnica (okolje fun) #:transparent)
(struct funkcija (ime telo) #:transparent)
(struct klici (ovojnica) #:transparent)

(define (jais2 e)
  (letrec ([jais
            (lambda (e env)
              (cond [(funkcija? e) (ovojnica env e)]
                    [(klici? e)
                     (let ([o (jais (klici-ovojnica e) env)])
                       (if (ovojnica? o)
                           (jais (funkcija-telo (ovojnica-fun o)) (ovojnica-okolje o))
                           (error "klic funkcije ni ustrezen")))]
                    [(konst? e) e]
                    [(bool? e) e]
                    [(shrani? e) (jais (shrani-izraz e) (jais (shrani-vrednost e) env))]
                    [(beri? e) env]
                    [(sestej? e)
                     (let ([v1 (jais (sestej-e1 e) env)]
                           [v2 (jais (sestej-e2 e) env)])
                       (if (and (konst? v1) (konst? v2))
                           (konst (+ (konst-int v1) (konst-int v2)))
                           (error "sestevanec ni konstanta")))]
                    [(negiraj? e)
                     (let ([v (jais (negiraj-e e) env)])
                       (cond [(bool? v) (bool (not (bool-b v)))]
                             [(konst? v) (konst (- (konst-int v)))]
                             [#t (error "negacija nepricakovanega izraza!")]))]
                    [(ce-potem-sicer? e)
                     (let ([test (jais (ce-potem-sicer-pogoj e) env)])
                       (if (bool? test)
                           (if (bool-b test)
                               (jais (ce-potem-sicer-res e) env)
                               (jais (ce-potem-sicer-nires e) env))
                           (error "pogoj ni logicna vrednost")))]
                    [#t (error "sintaksa izraza ni pravilna")]))])
    (jais e null)))
