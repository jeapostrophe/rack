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

(define int? exact-nonnegative-integer?)

(define-types
  [type
   void
   [atom
    [  int 1 8 16 32 64]
    [float     16 32 64 80 128]
    [(ptr [ref type?])]]
   [(vector [elem type:atom?])
    2 4 8 16]
   [(array [elem type?] (num int?))]
   [(struct [elems (vectorof type?)] [packed? boolean?])]
   [(fun [args (vectorof type?)] [ret type?])]]
  [expr
   [val
    [(int [v exact-nonnegative-integer?])
     1 8 16 32 64]
    [(float [v flonum?])     
     16 32 64 80 128]
    [null]
    ;; xxx check size
    [(vector [vs (vectorof expr?)])
     2 4 8 16]
    [(array [vs (vectorof expr?)])]
    [(struct [elts (vectorof expr?)])]
    [zero]]
   ;; i = integer, f = floating, ff = "fast" floating, u = unsigned, s = signed
   [(iadd [lhs expr?] [rhs expr?])]
   [(fadd [lhs expr?] [rhs expr?])]
   [(ffadd [lhs expr?] [rhs expr?])]

   [(isub [lhs expr?] [rhs expr?])]
   [(fsub [lhs expr?] [rhs expr?])]
   [(ffsub [lhs expr?] [rhs expr?])]

   [(imul [lhs expr?] [rhs expr?])]
   [(fmul [lhs expr?] [rhs expr?])]
   [(ffmul [lhs expr?] [rhs expr?])]

   [(iudiv [lhs expr?] [rhs expr?])]
   [(isdiv [lhs expr?] [rhs expr?])]
   [(fdiv [lhs expr?] [rhs expr?])]
   [(ffdiv [lhs expr?] [rhs expr?])]

   [(iurem [lhs expr?] [rhs expr?])]
   [(isrem [lhs expr?] [rhs expr?])]
   [(frem [lhs expr?] [rhs expr?])]
   [(ffrem [lhs expr?] [rhs expr?])]

   [(ishl [lhs expr?] [rhs expr?])]
   [(ilshr [lhs expr?] [rhs expr?])]
   [(iashr [lhs expr?] [rhs expr?])]
   [(iand [lhs expr?] [rhs expr?])]
   [(iior [lhs expr?] [rhs expr?])]
   [(ixor [lhs expr?] [rhs expr?])]

   [(vec-ref [vec expr?] [idx expr?])]
   [(vec-set [vec expr?] [idx expr?] [elt expr?])]
   [(vec-shuffle [lhs expr?] [rhs expr?]
                 [mask (vectorof int?)])]

   [(agg-ref [agg expr?]
             [path (vectorof int?)])]
   [(agg-set [agg expr?]
             [path (vectorof int?)]
             [elt expr?])]

   [(alloca [ty type?] [num expr?])]
   [(load [ptr expr?])]

   [(ptr-addr [ptr expr?] [path (vectorof expr?)])]
   [(ptr-vec-addrs [ptr-vec expr?] [idx expr?])]

   [(itrunc [val expr?] [ty type?])]
   [(izext [val expr?] [ty type?])]
   [(isext [val expr?] [ty type?])]

   [(ftrunc [val expr?] [ty type?])]
   [(fext [val expr?] [ty type?])]

   [(f->iu [val expr?] [ty type?])]
   [(f->is [val expr?] [ty type?])]
   [(iu->f [val expr?] [ty type?])]
   [(is->f [val expr?] [ty type?])]
   ;; xxx should i include these?
   [(ptr->i [val expr?] [ty type?])]
   [(i->ptr [val expr?] [ty type?])]

   [(icmp [lhs expr?] [rhs expr?])
    eq ne ugt uge ult ule sgt sge slt sle]
   [(fcmp [lhs expr?] [rhs expr?])
    false oeq ogt oge olt ole one ord ueq ugt uge ult ule une uno true]

   [(select [cond expr?] [true expr?] [false expr?])]
   ;; xxx support tail (can't see alloca), readonly, readnone [always
   ;; generate nounwind]
   [(call [fun expr?] [args (vectorof expr?)])
    ext int]

   [(let [id symbol?] [val expr?] [body expr?])]
   [(local-ref [id symbol?])]
   [(global-ref [id symbol?])]]
  [stmt
   [(switch [cond expr?] [default int?] [cases (vectorof (hash/c int? stmt?))])]
   [(store [ptr expr?] [val expr?] [cont stmt?])]
   [(let [id symbol?] [val expr?] [body stmt?])]
   [(ret [val expr?])]]
  [defn
    ;; xxx constant
    [(var [ty type?] [id symbol?])
     [(int [val constant?])]
     [ext]]
    [(fun [ret type?] [id symbol?] [args (vectorof (vector/c symbol? type?))])
     ;; generate nounwind
     ;; xxx [readnone, readonly]
     [(int [body stmt?])]
     [ext]]]
  [(prog [defns (listof defn?)])])

(define (constant? e)
  (error 'constant?))

