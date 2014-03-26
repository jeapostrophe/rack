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
(define int-width/c (one-of/c 1 8 16 32 64))
(define float-width/c (one-of/c 16 32 64 80 128))
;; TODO almost every type check has to account for vectors
;; TODO 2 4 8 16
(define vec-len/c (one-of/c 2 4 8 16))
(define (vector/len/c len/c elt/c)
  (λ (x)
    (and (vector? x)
         (len/c (vector-length x))
         (for/and ([e (in-vector x)])
           (elt/c e)))))

(define-types
  [type
   void
   [atom
    [(int [w int-width/c])]
    [(float [w float-width/c])]
    [(ptr [ref type?])]]
   [(vector [len vec-len/c] [elem type:atom?])]
   [(array [elem type?] (num int?))]
   [(struct [elems (vectorof type?)])]
   [(fun [args (vectorof type?)] [ret type?])]]
  [expr
   [val
    [(int [w int-width/c] [v exact-integer?])]
    [(float [w float-width/c] [v flonum?])]
    [null] ;; TODO
    [(vector [vs (vector/len/c vec-len/c expr?)])] ;; TODO
    [(array [vs (vectorof expr?)])] ;; TODO
    [(struct [elts (vectorof expr?)])] ;; TODO
    [zero]] ;; TODO

   [(int [lhs expr?] [rhs expr?])
    ;; u = unsigned, s = signed
    sub mul udiv sdiv urem srem
    shl lshr ashr and ior xor add]

   [(float [lhs expr?] [rhs expr?])
    add  sub  mul  div  rem]

   [(vec-ref [vec expr?] [idx expr?])] ;; TODO
   [(vec-set [vec expr?] [idx expr?] [elt expr?])] ;; TODO
   [(vec-shuffle [lhs expr?] [rhs expr?]
                 [mask (vectorof int?)])] ;; TODO

   [(agg-ref [agg expr?]
             [path (vectorof int?)])] ;; TODO
   [(agg-set [agg expr?]
             [path (vectorof int?)]
             [elt expr?])] ;; TODO

   [(alloca [ty type?] [num expr?])] ;; TODO
   [(load [ptr expr?])] ;; TODO

   ;; GEP
   [(ptr-addr [ptr expr?] [path (vectorof expr?)])] ;; TODO
   [(ptr-vec-addrs [ptr-vec expr?] [idx expr?])] ;; TODO

   [(convert [val expr?] [ty type?])
    [i2i
     trunc zext sext]
    [f2f
     trunc ext]
    [f2i
     u s]
    [i2f
     u s]]

   [(icmp [lhs expr?] [rhs expr?])
    eq ne ugt uge ult ule sgt sge slt sle]  ;; TODO
   [(fcmp [lhs expr?] [rhs expr?])
    false oeq ogt oge olt ole one ord ueq ugt uge ult ule une uno true]  ;; TODO

   [(select [cond expr?] [true expr?] [false expr?])]  ;; TODO
   ;; xxx support tail (can't see alloca), readonly, readnone [always
   ;; generate nounwind]
   [(call [fun expr?] [args (vectorof expr?)])
    ext
    int]  ;; TODO

   [(let [id symbol?] [val expr?] [body expr?])]
   [(local-ref [id symbol?])]
   [(global-ref [id symbol?])]]
  [stmt
   [(switch [cond expr?] 
            [cases (hash/c int? stmt?)]
            [default stmt?])]  ;; TODO
   [(store [ptr expr?] [val expr?] [cont stmt?])] ;; TODO
   [(let [id symbol?] [val expr?] [body stmt?])]
   [(ret [val expr?])]]
  [defn
    ;; xxx constant
    [(var [ty type?] [id symbol?])
     [(int [val constant?])]  ;; TODO
     [extern]]  ;; TODO
    [(fun [ret type?] [id symbol?] [args (vectorof (vector/c symbol? type?))])
     ;; generate nounwind
     ;; xxx [readnone, readonly]
     [(local [body stmt?])
      int ;; TODO
      ext]
     [extern]]]
  [(prog [defns (listof defn?)])])

(define (constant? e)
  (error 'constant?))
