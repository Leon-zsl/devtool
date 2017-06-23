#lang racket

(define rst (((λ (le)
                ((λ (mk-length)
                   (mk-length mk-length))
                 (λ (mk-length)
                   (le (λ (x)
                         ((mk-length mk-length) x))))))
              (λ (length)
                (λ (l)
                  (cond
                    [(null? l) 0]
                    [else (+ 1 (length (cdr l)))]))))
             '(1 2 3 4 5 6 7 8)))
(displayln rst)

; Applicative-order Y-Combinator
(define (Y g)
  ((λ (f) (f f))
   (λ (f)
     (g (λ (x)
          ((f f) x))))))

;key point:
;(lambda (x) ((f f) x)) is lazy version of (f f), in which f is
;(lambda (f) (g (lambda (x) ((f f) x)))), and (f f) is (Y g)
;so (lambda (x) ((f f) x)) is lazy version of (Y g)
;so evalute (Y g) => evalute (g (Y g)), in which the 2nd (Y g) is lazy version
 
(define rst1 ((Y
               (λ (len)
                 (λ (l)
                   (cond
                     [(null? l) 0]
                     [else (+ 1 (len (cdr l)))]))))
              '(1 2 3 4 5 6)))
(displayln rst1)

;(define rst2 (Y Y))
;(display rst2)
;inifinity in strict version