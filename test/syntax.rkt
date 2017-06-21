#!/usr/bin/env racket

#lang racket

(define-syntax my-rec
  (syntax-rules ()
    ((_ x e) (letrec ([x e]) x))))

(map (my-rec sum
             (λ (x)
               (if (= x 0)
                   0
                   (+ x (sum (- x 1))))))
     '(0 1 2 3 4))

(define-syntax my-let
  (syntax-rules ()
    [(_ ((p e) ...) b1 b2 ...)
     ((λ (p ...) b1 b2 ...) e ...)]
    [(_ n ((p e) ...) b1 b2 ...)
     (my-rec n ((λ (p ...) b1 b2 ...) e ...))]))

(my-let ([a 1] [b 2])
        (+ a b))

(define-syntax my-let-1
  (λ (x)
    (syntax-case x ()
      [(_ ((p e) ...) b1 b2 ...)
       #'((λ (p ...) b1 b2 ...) e ...)]
      [(_ n ((p e) ...) b1 b2 ...)
       (identifier? #'n)
       #'(my-rec n ((λ (p ...) b1 b2 ...) e ...))])))

(my-let-1 ((a 1) (b 2))
          (* a b))

(let-syntax ([if (λ (x)
                   (syntax-case x ()
                     [(_ e1 e2 e3)
                      #'(if e1 e2 e3)]))])
  (if (= 1 2) "yes" "no"))

(let-syntax ([divide (λ (x)
                       (let ([t -])
                         (syntax-case x ()
                           [(_ a b) #`(#,t a b)])))])
  (let ([t *]) (divide 5 2)))

(let-syntax ([multi (λ (x)
                      (syntax-case x ()
                        [(k a b)
                         ;(let ([s (datum->syntax #'k 't)])
                         (let ([s (datum->syntax #'k 't)])
                           #`(#,s a b))]))])
  (let ([t *]) (multi 5 2)))


(define-syntax define-integrable
  (syntax-rules (lambda)
    [(_ name (lambda formals form1 form2 ...))
     (begin
       (define xname (lambda formals form1 form2 ...))
       (define-syntax name
         (lambda (x)
           (syntax-case x ()
             (printf "expander name:~n")
             [_ (identifier? x) #'xname]
             [(_ arg (... ...))
              #'(xname
                 arg
                 (... ...))])))
       (printf "expander:~n"))]))

(define-integrable my-add (lambda (x y) (+ x y)))
;(my-add 2 4)

#|
(define-syntax define-integrable
  (syntax-rules (lambda)
    [(_ name (lambda formals form1 form2 ...))
     (begin
       (define xname (lambda formals form1 form2 ...))
       (define-syntax name
         (lambda (x)
           (syntax-case x ()
             [_ (identifier? x) #'xname]
             [(_ arg (... ...))
              #'((lambda formals form1 form2 ...)
                 arg
                 (... ...))]))))]))


(define-integrable my-add (lambda (x y) (+ x y)))
|#

(define-syntax define-structure
  (lambda (x)
    (define gen-id
      (lambda (template-id . args)
        (datum->syntax template-id
                       (string->symbol
                        (apply string-append
                               (map (lambda (x)
                                      (if (string? x)
                                          x
                                          (symbol->string (syntax->datum x))))
                                    args))))))
    (syntax-case x ()
      [(_ name field ...)
       (with-syntax ([constructor (gen-id #'name "make-" #'name)]
                     [predicate (gen-id #'name #'name "?")]
                     [(access ...)
                      (map (lambda (x) (gen-id x #'name "-" x))
                           (syntax->list #'(field ...)))]
                     [(assign ...)
                      (map (lambda (x) (gen-id x "set-" #'name "-" x "!"))
                           (syntax->list #'(field ...)))]
                     [structure-length (+ (length (syntax->list #'(field ...))) 1)]
                     [(index ...)
                      (let f ([i 1] [ids (syntax->list #'(field ...))])
                        (if (null? ids)
                            '()
                            (cons i (f (+ i 1) (cdr ids)))))])
         #'(begin
             (define constructor
               (lambda (field ...)
                 (vector 'name field ...)))
             (define predicate
               (lambda (x)
                 (and (vector? x)
                      (= (vector-length x) structure-length)
                      (eq? (vector-ref x 0) 'name))))
             (define access
               (lambda (x)
                 (vector-ref x index)))
             ...
             (define assign
               (lambda (x update)
                 (vector-set! x index update)))
             ...
             ))])))

(define-structure tree left right)
(define t (make-tree 1 2))
(tree-left t)
(displayln t)

(define-syntax define-structure-simple
  (lambda (x)
    (syntax-case x ()
      [(_ name field ...)
       (with-syntax ([args (map (lambda (f) f)
                                (syntax->list #'(field ...)))])
         #''(name . args))])))

(define st (define-structure-simple tree1 left1 right1))
(displayln st)
