#lang racket/base
(require racket/contract
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     racket/string
                     syntax/parse))

(begin-for-syntax
  (define-syntax-class tyi
    #:attributes (name (fields 1) (ctcs 1))
    (pattern (~or name:id name:number)
             #:attr (fields 1) empty
             #:attr (ctcs 1) empty)
    (pattern (name:id [fields:id ctcs:expr] ...)))
  (define-syntax-class tys
    #:attributes (spec)
    (pattern p:tyi
             #:attr spec #'(p.name (p.fields ...) (p.ctcs ...))))
  (define-syntax-class ty
    #:attributes ((specs 1))
    (pattern
     [p:tys c:ty ...]
     #:attr (specs 1)
     (cons (list (attribute p.spec))
           (map (λ (s) (cons (attribute p.spec) s))
                (append* (attribute c.specs)))))
    (pattern
     p:tys
     #:attr (specs 1)
     (list (list (attribute p.spec))))))

(define-syntax (define-types stx)
  (syntax-parse stx
    [(_ t:ty ...)
     (let ()
       (with-syntax ([(s ...) (append* (attribute t.specs))])
         (quasisyntax/loc stx
           (begin (define-type . s)
                  ...))))]))

(begin-for-syntax
  (define (id-join stx l sep)
    (format-id stx "~a"
               (string-join
                (map (λ (x) (format "~a" x)) (syntax->datum l))
                sep))))

(define-syntax (define-type stx)
  (syntax-parse stx
    [(_ (parent0:id (field0:id ...) (ctc0:expr ...)))
     (syntax/loc stx
       (begin (struct parent0 (field0 ...) #:transparent)
              (provide
               (contract-out
                [struct parent0 ([field0 ctc0] ...)]))))]
    [(_ (parent0:id (field0:id ...) (ctc0:expr ...))
        (parentN:id (fieldN:id ...) (ctcN:expr ...)) ...
        (child (field:id ...) (ctc:expr ...)))
     (with-syntax
         ([parent:child
           (id-join #'child #'(parent0 parentN ... child) ":")]
          [parents
           (id-join #'parent0 #'(parent0 parentN ...) ":")])
       (syntax/loc stx
         (begin (struct parent:child parents (field ...) #:transparent)
                (provide
                 (contract-out
                  [struct parent:child
                          ([field0 ctc0] ...
                           [fieldN ctcN] ... ...
                           [field ctc] ...)])))))]))

(provide
 define-types)
