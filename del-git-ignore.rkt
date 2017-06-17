#!/usr/bin/env racket

#lang racket

(define (find-and-delete-git-ignore in)
  (define line (read-line in 'any))
  (if (not (eof-object? line))
      (let* ([file (string-trim line)]
             [non-empty? (non-empty-string? file)]
             [exist? (and non-empty? (or (file-exists? file) (directory-exists? file)))])
        (if exist?
            ;(begin
            ;  (delete-directory/files file)
            ;  (printf "delete ignored file or dir:~a~n" file))
            (delete-directory/files file)
            (void))
        (find-and-delete-git-ignore in))
      (void)))

(define (start-del-git-ignore path)
  (printf "del-git-ignore:~a~n" path)
  (if (and (directory-exists? path) (directory-exists? (build-path path ".git")))
      (let ([ignore-list-tmp-file ".tmp.ignore"])
        (system (string-append "find " path " | git check-ignore --stdin > " ignore-list-tmp-file))
        (let ([in (open-input-file ignore-list-tmp-file #:mode 'text)])
          (find-and-delete-git-ignore in)
          (close-input-port in))
        (if (file-exists? ignore-list-tmp-file) (delete-file ignore-list-tmp-file) (void)))
      (error "path does not exist or not a git path:" path)))

(require racket/cmdline)
(define (main)
  (with-handlers ([exn:fail? (λ (exn) (printf "exn found:~a" exn))])
    (command-line
     #:program "del-git-ignore"
     #:args (path)
     (start-del-git-ignore path))))
(main)
