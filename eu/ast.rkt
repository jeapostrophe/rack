#lang racket/base
(require racket/contract
         rack/etc/define-types)

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
