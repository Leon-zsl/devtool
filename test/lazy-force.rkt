#lang racket

(define count 0)

(define (stream-car s)
  (car (force s)))

(define (stream-cdr s)
  (cdr (force s)))

(define counters
  (let next ([n 1])
    (delay (begin
             (set! count (+ count 1))
             (cons n (next (+ n 1)))))))
