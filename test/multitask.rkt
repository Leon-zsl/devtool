#lang r6rs

(library (tspl engines)
         (export make-engine timed-lambda)
         (import (rnrs) (racket trace))
         
         (define clock 0)
         (define handler #f)
         (define start-timer
           (lambda (ticks new-handler)
             (display "start timer\n")
             (set! handler new-handler)
             (set! clock ticks)))
         (define stop-timer
           (lambda ()
             (let ([time-left clock])
               (display "stop timer\n")
               (set! clock 0)
               time-left)))
         (define decrement-timer
           (lambda ()
             (when (> clock 0)
               (display "decrement timer\n")
               (set! clock (- clock 1))
               (when (= clock 0) (handler)))))
         
         (define make-engine
           (let ([do-complete #f] [do-expire #f])
             (display "make engine\n")
             (define timer-handler
               (lambda ()
                 (display "timer handler\n")
                 (start-timer (call/cc do-expire) timer-handler)))
             (define new-engine
               (lambda (resume)
                 (lambda (ticks complete expire)
                   ((call/cc
                     (lambda (escape)
                       (display escape)
                       (display "\n")
                       (set! do-complete
                             (lambda (ticks value)
                               (display "do-complete\n")
                               (escape (lambda () (complete ticks value)))
                               ;(lambda () (display "invoke complete\n")
                               ;  (complete ticks value))
                               ))
                       (set! do-expire
                             (lambda (resume)
                               (display "do expire\n")
                               (escape (lambda ()
                                         (expire (new-engine resume))))))
                       (display "engine body\n")
                       (resume ticks)))))))
             (lambda (proc)
               (new-engine
                (lambda (ticks)
                  (display "engine proc\n")
                  (start-timer ticks timer-handler)
                  (let ([value (proc)])
                    (let ([ticks (stop-timer)])
                      (display "complete engine\n")
                      (do-complete ticks value))))))))
         
         (define-syntax timed-lambda
           (syntax-rules ()
             [(_ formals exp1 exp2 ...)
              (lambda formals (decrement-timer) exp1 exp2 ...)]))

         (define fibonacci
           (lambda (n)
             (display "fib:\n")
             (if (< n 2)
                 n
                 (+ (fibonacci (- n 1))
                    (fibonacci (- n 2))))))
         (define eng
           (make-engine
            (lambda ()
              (fibonacci 3))))
         
         (define eng1 (eng 5
                           (lambda (ticks value) (display "complete 1\n") value)
                           (lambda (new-eng)
                             (display "expired 1\n")
                             (set! eng new-eng))))
         (display eng1)
         (display "\n")
         (define eng2 (eng 5
                           (lambda (ticks value) (display "complete 2\n") value)
                           (lambda (new-eng)
                             (display "expired 2\n")
                             (set! eng new-eng))))
         (display eng2)
         (display "\n")
         (display "load finish"))

