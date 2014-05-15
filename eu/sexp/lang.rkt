#lang racket/base
(require rack/eu/ast
         rack/eu/main
         racket/list
         (for-syntax racket/base
                     racket/pretty
                     syntax/parse))

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ d ...)
     (pretty-print (syntax->datum #'(d ...)))
     (syntax/loc stx
       (#%module-begin 
        (define src
          (prog (flatten (list d ...))))
        (verify src)
        (define mod
          (compile src))
        (module+ main
          (run mod))
        (module+ dump
          (dump mod))        
        (module+ compile
          (require racket/cmdline)
          (command-line #:args (file)
                        (emit mod file)))))]))

(provide
 (all-from-out rack/eu/ast)
 (except-out (all-from-out racket/base)
             #%module-begin)
 (rename-out
  [module-begin #%module-begin]))
