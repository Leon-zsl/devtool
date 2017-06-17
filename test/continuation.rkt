#lang racket

(define cc #f)
(define rst 0)

(define f1
  (lambda (x)
    (printf "x:~a~n" x)
    (define v (+ 1 (call/cc
                    (lambda (k)
                      (printf "k:~a~n" k)
                      (set! cc (compose k (curry * 5)))
                      (* 2 (k x))
                      ;(k x)
                      (printf "cc:~a~n" cc)
                      ;(k printf)
                      (set! rst 1)
                      ;(* 2 x)
                      ;x
                      ))))
    (printf "v:~a~n" v)
    v))


(printf "func invoke~n")
(printf "f1:~a,~a~n" (f1 3) rst)
(printf "cc invoke~n")
(printf "again:~a,~a~n" (cc 6) rst)