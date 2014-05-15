#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/pretty)
         (prefix-in sexp: rack/eu/sexp/lang)
         racket/stxparam
         racket/list
         rack/eu/ast)

(define-syntax (module-begin stx)
  (pretty-print (syntax->datum stx))
  (syntax-parse stx
    [(_ e ...)
     (syntax/loc stx
       (sexp:#%module-begin e ...))]))

(define-syntax (code stx)
  (syntax-parse stx
    [(_ . content)
     (syntax/loc stx
       content)]))

(begin-for-syntax
  (define (seq-error stx)
    (raise-syntax-error '|;| "illegal use of ;" stx))
  (define (seq-top stx)
    (syntax-parse stx
      [(_ e ...)
       (syntax/loc stx
         (list e ...))]))
  (define (seq-body stx)
    (syntax-parse stx
      [(_ e)
       (syntax/loc stx
         e)]
      [(_ a d)
       (syntax/loc stx
         (ctxt:let (gensym) a d))])))

(define-syntax-parameter |;|
  seq-top)

(define-syntax (extern stx)
  (syntax-parse stx
    [(_ return-ty id args)
     (syntax/loc stx
       (defn:fun:extern (compile-type return-ty) 'id (compile-id+type-seq args)))]
    [(_ return-ty id args (#%braces . body))
     (syntax/loc stx
       (defn:fun:local:ext (compile-type return-ty) 'id (compile-id+type-seq args)
         (syntax-parameterize ([|;| seq-body])
           (maybe-make-stmt body))))]))

(define (ctxt:let id val body)
  (if (stmt? body)
    (stmt:let id val body)
    (expr:let id val body)))

(define (maybe-make-stmt t)
  (if (stmt? t)
    t
    (stmt:ret t)))

(define-syntax (compile-type stx)
  (syntax-parse stx
    [(_ (~datum int32)) (syntax/loc stx (type:atom:int 32))]))
(define-syntax (compile-id+type-seq stx)
  (syntax-parse stx
    [(_ ())
     (syntax/loc stx (vector))]
    [(_ (type id))
     (syntax/loc stx (vector (vector 'id (compile-type type))))]))

(define-syntax (ext stx)
  (syntax-parse stx
    [(_ fun arg ...)
     (syntax/loc stx
       (expr:call:ext (expr:global-ref 'fun) (vector arg ...)))]))

(define-syntax (compile-let stx)
  (syntax-parse stx
    [(_ (#%brackets (~datum =) id val) body)
     (syntax/loc stx
       (expr:let 'id val body))]))

(define-syntax (#%braces stx)
  (syntax-parse stx
    [(_ . more)
     (syntax/loc stx more)]))

(define-syntax (top stx)
  (syntax-parse stx
    [(_ . id)
     (syntax/loc stx (expr:local-ref 'id))]))

(define-syntax (datum stx)
  (syntax-parse stx
    [(_ . x:str)
     (syntax/loc stx empty)]
    [(_ . n)
     #:when (number? (syntax->datum #'n))
     ;; xxx size
     (syntax/loc stx (expr:val:int (#%datum . 32) (#%datum . n)))]))

(provide
 (rename-out [top #%top]
             [module-begin #%module-begin]
             [compile-let let]
             [datum #%datum])
 (rename-out [expr:int:mul |*i|]
             [expr:int:add |+i|])
 code
 #%braces
 |;|
 extern
 ext)
