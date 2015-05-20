#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/list
         racket/generic
         racket/match)

(define-generics ll-formatable
  (ll-format ll-formatable))

;; xxx make something similar to create it in-memory
(define (ll-out t)
  (match t
    [(or '() #f (? void?))
     (void)]
    [(cons a d)
     (ll-out a)
     (display #\space)
     (ll-out d)]
    [(or (? char?) (? string?))
     (display t)]
    [_
     (ll-out (ll-format t))]))

(define-syntax (define-ll-type stx)
  (syntax-parse stx
    [(_ (name:id arg:id ...)
        format-e:expr)
     (with-syntax ([*name (format-id #'name "*~a" #'name)]
                   [ty:name (format-id #'name "ty:~a" #'name)]
                   [*name? (format-id #'name "*~a?" #'name)]
                   [ty:name? (format-id #'name "ty:~a?" #'name)])
       (syntax/loc stx
         (begin
           (struct *name (arg ...)
             #:methods gen:ll-formatable
             [(define (ll-format v)
                (match-define (*name arg ...) v)
                format-e)])
           (define HASH (make-weak-hash))
           (define (ty:name arg ...)
             (hash-ref! HASH (list arg ...)
                        (λ ()
                          (*name arg ...))))
           (define (ty:name? x)
             (*name? x)))))]))

(define-ll-type (void)
  "void")
(define-ll-type (fun params return)
  (list return "(" params ")"))
(define-ll-type (i1) "i1")
(define-ll-type (i8) "i8")
(define-ll-type (i16) "i16")
(define-ll-type (i32) "i32")
(define-ll-type (i64) "i64")
(define-ll-type (f32) "float")
(define-ll-type (f64) "double")
(define-ll-type (ptr to)
  (list to "*"))
(define-ll-type (vector count of)
  ;; xxx what does llvm do if count isn't supported by hardware?
  (list "<" (number->string count) "x" of ">"))
(define-ll-type (array count of)
  (list "[" (number->string count) "x" of "]"))
(define-ll-type (struct contents)
  (list "{" (add-between contents ",") "}"))

(define (ty? x)
  (or (ty:void? x) (ty:addressable? x)))
(define (ty:addressable? x)
  (or (ty:fun? x) (ty:first-class? x)))
(define (ty:first-class? x)
  (or (ty:single-value? x)
      (ty:aggregate? x)))
(define (ty:single-value? x)
  (or (ty:integer? x) (ty:float? x) (ty:ptr? x) (ty:vector? x)))
(define (ty:integer? x)
  (or (ty:i1? x) (ty:i8? x) (ty:i16? x) (ty:i32? x) (ty:i64? x)))
(define (ty:float? x)
  (or (ty:f32? x) (ty:f64? x)))
(define (ty:aggregate? x)
  (or (ty:array? x) (ty:struct? x)))

(define-generics ll-typeable
  (ll-ty ll-typeable))

(define-syntax (define-ll-val stx)
  (syntax-parse stx
    [(_ (name:id arg:id ...)
        #:ty type-e:expr
        #:fmt format-e:expr)
     (with-syntax ([val:name (format-id #'name "val:~a" #'name)]
                   [val:name? (format-id #'name "val:~a?" #'name)])
       (syntax/loc stx
         (struct val:name (arg ...)
           #:methods gen:ll-formatable
           [(define (ll-format v)
              (match-define (val:name arg ...) v)
              format-e)]
           #:methods gen:ll-typeable
           [(define (ll-ty v)
              (match-define (val:name arg ...) v)
              type-e)])))]))

(define-ll-val (i1 n)
  #:ty (ty:i1)
  #:fmt (if (= n 0) "false" "true"))
(define-ll-val (i8 n)
  #:ty (ty:i8)
  #:fmt (number->string n))
(define-ll-val (i16 n)
  #:ty (ty:i16)
  #:fmt (number->string n))
(define-ll-val (i32 n)
  #:ty (ty:i32)
  #:fmt (number->string n))
(define-ll-val (i64 n)
  #:ty (ty:i64)
  #:fmt (number->string n))
(define-ll-val (f32 n)
  #:ty (ty:f32)
  #:fmt (number->string n))
(define-ll-val (f64 n)
  #:ty (ty:f64)
  #:fmt (number->string n))
(define-ll-val (null to)
  #:ty (ty:ptr to)
  #:fmt "null")

(define (ll-ty+val-list l)
  (add-between (map (λ (v) (list (ll-ty v) v)) l) ","))

(define-ll-val (struct contents)
  #:ty (ty:struct contents)
  #:fmt (list "{" (ll-ty+val-list contents) "}"))
(define-ll-val (array of contents)
  #:ty (ty:array (length contents) of)
  #:fmt (list "[" (ll-ty+val-list contents) "]"))
(define-ll-val (vector of contents)
  #:ty (ty:array (length contents) of)
  #:fmt (list "<" (ll-ty+val-list contents) ">"))
(define-ll-val (zero of)
  #:ty of
  #:fmt "zeroinitializer")

;; xxx other value: pointer to global

(define (val:true)
  (val:i1 1))
(define (val:false)
  (val:i1 0))
(define (val:bool b)
  (if b (val:true) (val:false)))

;; xxx incorrect for unicode
(define (val:string s)
  (val:array (ty:i8) (map (λ (c) (val:i8 (char->integer c))) (string->list s))))

(define (val? x)
  (or (val:simple? x) (val:complex? x)))
(define (val:simple? x)
  (or (val:integer? x) (val:float? x) (val:null? x)))
(define (val:integer? x)
  (or (val:i1? x) (val:i8? x) (val:i16? x) (val:i32? x) (val:i64? x)))
(define (val:float? x)
  (or (val:f32? x) (val:f64? x)))
(define (val:complex? x)
  (or (val:struct? x) (val:array? x) (val:vector? x) (val:zero? x)))

(module+ test
  (ll-out
   (list
    "@.str = private unnamed_addr constant "
    (ty:array 13 (ty:i8)) (val:string "Hello World!\n\0") "\n"

    "declare " (ty:i32) " @puts(" (ty:ptr (ty:i8)) " nocapture) nounwind" "\n"

    "define " (ty:i32) " @main() {
    %cast210 = getelementptr " (ty:ptr (ty:array 13 (ty:i8))) " @.str, " (ty:i64) (val:i64 0)", " (ty:i64) (val:i64 0)"
    call "(ty:i32)" @puts("(ty:ptr (ty:i8))" %cast210)
    ret "(ty:i32) (val:i32 0)"
   }" "\n"

    "define "(ty:i32)" @square_unsigned("(ty:i32)" %a) {
    %1 = mul "(ty:i32)" %a, %a
    ret "(ty:i32)" %1
   }" "\n"

    "define "(ty:vector 4 (ty:i32)) "@multiply_four("(ty:vector 4 (ty:i32))" %a, "(ty:vector 4 (ty:i32))" %b) {
    %1 = mul "(ty:vector 4 (ty:i32))" %a,  %b
    ret "(ty:vector 4 (ty:i32))" %1
   }" "\n")))

(module+ notes
  '[Instructions
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
  )
