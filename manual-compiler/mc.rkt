#lang racket/base

[Types
 (void)
 [Addressable
  (fun params return)
  [First-Class
   [Single-Value
    [Integer
     (i1) (i8) (i16) (i32) (i64)]
    [Float
     (f32) (f64)]
    (ptr to)
    ;; xxx what does llvm do if count isn't supported by hardware?
    (vector count of)]
   [Aggregate
    (array count of)
    (struct content ...)]]]]

[Constants
 [Simple
  [Integer
   (i1 n) (i8 n) (i16 n) (i13 n) (i64 n)]
  [Float
   (f32 n) (f64 n)]
  (null)]
 [Complex
  (struct content ...)
  (array content ...)
  (vector content ...)
  (zero-initializer)]
 (ptr to-sym)]

(define (Constants:Simple:Integer:i1:false) (i1 0))
(define (Constants:Simple:Integer:i1:true) (i1 0))
(define (Constants:Complex:array:char s)
  (Constants:Complex:array (map char->integer (string->list s))))

[Instructions
 [Terminator
  (ret value-or-void)
  (cbr cond:i1 true:label false:label)
  (ubr dest:label)
  (switch value:int
          default:label
          [val:constant dest:label]
          ...)]
 [Binary
  ;; xxx replace these with the (unsigned/signed) "with overflow" versions
  (add left right)
  (fadd left right)
  (sub left right)
  (fsub left right)
  (mul left right)
  (fmul left right)
  (udiv left right)
  (sdiv left right)
  (fdiv left right)
  (urem left right)
  (srem left right)
  (frem left right)]
 [Bitwise
  (shl left right)
  (lshr left right)
  (ashr left right)
  (and left right)
  (or left right)
  (xor left right)]
 [Vector
  (extract-element vec idx)
  (insert-element vec val idx)
  ;; xxx may be useful to have undef for vec2
  (shuffle-vector vec1 vec2 mask)]
 [Aggregate
  (extract-value agg idx ...+)
  (insert-value agg val idx ...+)]
 [Memory
  (alloca ty)
  (load ptr)
  (store val ptr)
  (get-element-ptr ptr idx ...+)]
 [Conversion
  (trunc val to-ty)
  (zext val to-ty)
  (sext val to-ty)
  (fptrunc val to-ty)
  (fpext val to-ty)
  (fp->ui val to-ty)
  (fp->si val to-ty)
  (ui->fp val to-ty)
  (si->fp val to-ty)
  ;; xxx not including ptr<->int or bitcast
  ]
 [Other
  (icmp [mode '(eq ne ugt uge ult ule sgt sge slt sle)]
        left right)
  (fcmp [mode '(false oeq ogt oge olt ole one ord
                      ueq ugt uge ult ule une urd true)]
        left right)
  ;; xxx may only appear at the start of a block
  (phi [val label] ...)
  (select cond true false)
  ;; xxx external means ccc
  (call:external fnptr arg ...)
  ;; xxx internal means fastcc
  (call:internal fnptr arg ...)
  ;; xxx really a terminator (although ret has to come next) or don't
  ;; and always emit things with tailcall flag for internal
  (tail-call:internal fnptr arg ...)]]

;; xxx fma
;; xxx masked vector load/store
;; xxx masked vector gather/scatter

;; xxx let* + local-ref + global-ref

;; Module = definition ...
;; Definition = name -> (variable | extern | function | struct)
;; Variable = type, value, constant?
;; Extern = type, name
;; Function = type, block ...+
;; Struct = type
;; Block = label, phi nodes, instructions, terminator
