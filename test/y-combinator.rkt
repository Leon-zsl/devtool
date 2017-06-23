#lang r6rs

(library (y-combinator)
         (export test)
         (import (rnrs))

         (define test 0)
         (define test0 1)
         (set! test0 1)

         (define rst (((lambda (le)
                         ((lambda (mk-length)
                            (mk-length mk-length))
                          (lambda (mk-length)
                            (le (lambda (x)
                                  ((mk-length mk-length) x))))))
                       (lambda (length)
                         (lambda (l)
                           (cond
                             [(null? l) 0]
                             [else (+ 1 (length (cdr l)))]))))
                      '(1 2 3 4 5 6 7 8)))
         (display rst)
         (display "\n")

         (define (Y g)
           ((lambda (f) (f f))
            (lambda (f)
              (g (lambda (x)
                   ((f f) x))))))

         ;((Y g) '(1 2 3)) -> ((lambda (x) ((Y g) x)) '(1 2 3))
         ; so (Y g) is (lambda (x) ((Y g) x)) == (lambda (x) ((k k) x)) where k is (lambda (f) (g (lambda (x) ((f f) x))))
         ; in which (lambda (x) ((f f) x)) == (lambda (x) ((k k) x)) == (Y g)
         ; so (Y g) == (g (Y g))
         ; Applicative-order Y-Combinator
 
         (define rst1 ((Y
                        (lambda (len)
                          (lambda (l)
                            (cond
                              [(null? l) 0]
                              [else (+ 1 (len (cdr l)))]))))
                       '(1 2 3 4 5 6)))

         (display rst1)
         (display "\n")

         ;(define rst2 (Y Y))
         ;(display rst2)
         )