#!/usr/bin/env racket

#lang racket

(define (format-line-ending line)
  ; \r\n or \r -> \n
  (string-replace (string-replace line "\r\n" "\n") "\r" "\n"))

(define (format-tab line)
  ;4-space -> tab
  (string-replace line "    " "\t"))

(define (format-trim line)
  ;trim empty line
  (define trim-line (string-trim line))
  (if (equal? (string-length trim-line) 0)
      trim-line
      line))

(define (input-to-format-lines in)
  (define line (read-line in 'any))
  (if (eof-object? line)
      '()
      (let ([newline (format-trim (format-line-ending (format-tab line)))])
        (cons (string->bytes/utf-8 newline) (input-to-format-lines in)))))

(define (format-lines-to-output out lines)
  (cond
    [(> (length lines) 0)
     (write-bytes (car lines) out)
     (write-bytes (string->bytes/utf-8 "\n") out)
     (format-lines-to-output out (cdr lines))]
    [else (void)]))

(define (format-file file)
  (printf "format file:~a~n" file)
  (define in (open-input-file file #:mode 'binary))
  (define lines (input-to-format-lines in))
  (close-input-port in)
  (define out (open-output-file file #:mode 'binary #:exists 'truncate))
  (format-lines-to-output out lines)
  (close-output-port out))

(define (format-path path type-list recursive?)
  ;(printf "format path:~a~n" path)
  (for-each (λ (v)
              (define p (path->string v))
              (cond
                [(file-exists? p)
                 (if (or (not type-list)
                         (member p type-list string=?)
                         (and (path-get-extension p)
                              (let ([ext (string-trim (bytes->string/utf-8 (path-get-extension p)) ".")])
                                (member ext type-list string=?))))
                     (format-file p)
                     (void))]
                [recursive? (format-path p type-list recursive?)]))
            (directory-list path #:build? #t)))
    
(define (start-format path type recursive?)
  (printf "start parse...~npath:~a,type:~a,recursive:~a~n" path type recursive?)
  (define type-list (and type (string-split type ",")))
  (cond
    [(not (non-empty-string? path)) (error "path is empty")]
    [(file-exists? path) (format-file path)]
    [(directory-exists? path) (format-path path
                                           (map (λ (x) (string-trim x ".")) type-list)
                                           recursive?)]
    [else (error "path does no exist:" path)]))

;(start-parse "/Users/apple/Downloads/SplatBlit.cs" ".png,.html" #t)

(require racket/cmdline)
(define (main)
  (with-handlers ([exn:fail? (λ (exn) (printf "exn found:~a" exn))])
    (define file-type #f)
    (define recursive? #f)
    (command-line
     #:program "format"
     #:once-each
     (("-t" "--type") type "file type" (set! file-type type))
     (("-r" "--recursive") "recursive format" (set! recursive? #t))
     #:args (file-path)
    (start-format file-path file-type recursive?))))
(main)