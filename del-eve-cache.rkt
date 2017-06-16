#!/usr/bin/env racket

#lang racket

(define lib-folder '("/Users/apple/Library/Application Support/aoi"))

(define bundle-folder '("exp_res" "release" "code/unity/Assets/StreamingAssets/res"))

(define code-folder '("code/unity/Assets/Scripts/gds"
                      "code/unity/Assets/Scripts/proto"
                      "code/unity/Assets/Scripts/opcode"
                      "code/unity/Assets/Scripts/errcode"
                      "code/unity/Assets/Scripts/kvconfig"
                      "code/unity/Assets/Scripts/lang"
                      "code/unity/Assets/Scripts/redeemer"
                      "code/unity/Assets/Resources/loc/config/lang_conf.txt"
                      "code/unity/Assets/Resources/loc/loginfestring.txt"
                      "welib/errcode/.errcode_cs"
                      "welib/errcode/.errcode_lua"
                      "welib/opcode/.opcode_cs"
                      "welib/opcode/.opcode_lua"
                      "welib/proto/.cs"
                      "welib/proto/.lua"
                      "welib/gds/.exp_gds_schema_cs"
                      "welib/config/.kvconf_schema_cs"
                      "welib/redeemer/.exp_redeemer_schema_cs"
                      "welib/loc/.exp_loc"
                      "welib/loc/.exp_loc_cs"
                      "welib/locconf/.exp_loc_conf"
                      "welib/loclogin/.exp_login_loc"
                      "welib/loclogin/.exp_login_loc_cs"))

(define code-folder-without '(("code/unity/Assets/Bundles/lua_core" . (".ini" ".ini.meta"))
                              ("code/unity/Assets/Bundles/lua_game" . (".ini" ".ini.meta"))
                              ("code/unity/Assets/Bundles/loc" . (".ini" ".ini.meta"))
                              ("code/unity/Assets/Scripts/ToLua/Generate" . (".keepme"))))

(define cache-type '("lib" "bundle" "code"))

(define (del-file-or-dir p)
  ;(printf "del file or dir:~a~n" p)
  (define path (if (path? p) (path->string p) p))
  (cond
    [(file-exists? path) (delete-directory/files path)]
    [(directory-exists? path) (delete-directory/files path)]))

(define (del-lib-folder)
  (for-each (λ (p)
              ;(printf "del lib folder:~a~n" p)
              (del-file-or-dir p))
            lib-folder))

(define (del-bundle-folder path)
  (for-each (λ (p)
              ;(printf "del bundle folder:~a~n" p)
              (del-file-or-dir (build-path path p)))
            bundle-folder))

(define (del-code-folder path)
  (for-each (λ (p)
              ;(printf "del code folder:~a~n" p)
              (del-file-or-dir (build-path path p)))
            code-folder))

(define (del-code-folder-without path)
  (for-each (λ (pair)
              ;(printf "del code folder without:~a~n" pair)
              (define cp (build-path path (car pair)))
              (define ext-list (cdr pair))
              (cond [(directory-exists? cp)
                     (let ([path-list (directory-list cp #:build? #t)])
                       (for-each (λ (p)
                                   (if (not (foldl
                                             (λ (ext result)
                                               (or result
                                                   (string-suffix? (path->string p) ext)))
                                             #f
                                             ext-list))
                                       (del-file-or-dir p)
                                       (void)))
                                 path-list))]
                    [(file-exists? cp) (del-file-or-dir cp)]
                    [else (void)]))
            code-folder-without))

(define (start-del-eve-cache path type-string)
  (printf "delete eve cache...path:~a,type:~a~n" path type-string)
  (define type-list (or (and type-string (string-split type-string ",")) cache-type))
  (if (directory-exists? path)
      (begin
        (if (member "lib" type-list string=?) (del-lib-folder) (void))
        (if (member "bundle" type-list string=?) (del-bundle-folder path) (void))
        (if (member "code" type-list string=?)
            (begin
              (del-code-folder path)
              (del-code-folder-without path))
            (void)))
      (error "path does not exist:" path)))

(require racket/cmdline)
(define (main)
  (with-handlers ([exn:fail? (λ (exn) (printf "exn found:~a~n" exn))])
    (define type #f)
    (command-line
     #:program "del-eve-cache"
     #:once-each
     (("-t" "--type") t "delete type" (set! type t))
     #:args (path)
     (start-del-eve-cache path type))))
(main)
  