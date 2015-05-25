#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/list
         racket/generic
         racket/match)

(define-generics ll-formatable
  (ll-format ll-formatable))

(struct fmt:indent (l))
(struct fmt:newline ())

(define (fmt:between l v)
  (let loop ([l l])
    (match l
      ['() '()]
      [(list a) a]
      [(cons a d)
       (cons (vector a v) (loop d))])))

;; xxx make something similar to create it in-memory
(define (ll-out t)
  (define (loop i-level space-on-left? t)
    (match t
      [(or '() #f (? void?))
       space-on-left?]
      [(cons a d)
       (unless (loop i-level space-on-left? a)
         (display #\space))
       (loop i-level #t d)]
      [(? vector? v)
       (for/fold ([space-on-left? space-on-left?])
                 ([a (in-vector v)])
         (loop i-level space-on-left? a))]
      [(or (? char?) (? string?))
       (display t)
       #f]
      [(fmt:indent l)
       (define ends-in-space?
         (loop (add1 i-level) space-on-left?
               (cons (fmt:newline)
                     (fmt:between l (fmt:newline)))))
       (loop i-level ends-in-space? (fmt:newline))]
      [(fmt:newline)
       (newline)
       (for ([i (in-range i-level)])
         (display "  "))
       #t]
      [_
       (loop i-level space-on-left? (ll-format t))]))
  (loop 0 #f t)
  (void))

(define-syntax (define-ll-type stx)
  (syntax-parse stx
    [(_ (name:id arg:id ...)
        #:fmt format-e:expr)
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
  #:fmt "void")
(define-ll-type (fun params return)
  #:fmt (vector return "(" params ")"))
(define-ll-type (i1) #:fmt "i1")
(define-ll-type (i8) #:fmt "i8")
(define-ll-type (i16) #:fmt "i16")
(define-ll-type (i32) #:fmt "i32")
(define-ll-type (i64) #:fmt "i64")
(define-ll-type (f32) #:fmt "float")
(define-ll-type (f64) #:fmt "double")
(define-ll-type (ptr to)
  #:fmt (vector to "*"))
(define-ll-type (vector count of)
  ;; xxx what does llvm do if count isn't supported by hardware?
  #:fmt (vector "<" (list* (number->string count) "x" of) ">"))
(define-ll-type (array count of)
  #:fmt (vector "[" (list* (number->string count) "x" of) "]"))
(define-ll-type (struct contents)
  #:fmt (vector "{" (fmt:between contents ",") "}"))

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
     (with-syntax ([val:name (format-id #'name "val:~a" #'name)])
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
  (fmt:between (map (λ (v) (cons (ll-ty v) v)) l) ","))

(define-ll-val (struct contents)
  #:ty (ty:struct contents)
  #:fmt (vector "{" (ll-ty+val-list contents) "}"))
(define-ll-val (array of contents)
  #:ty (ty:array (length contents) of)
  #:fmt (vector "[" (ll-ty+val-list contents) "]"))
(define-ll-val (vector of contents)
  #:ty (ty:array (length contents) of)
  #:fmt (vector "<" (ll-ty+val-list contents) ">"))
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

(define-syntax (define-ll-inst stx)
  (syntax-parse stx
    [(_ (name:id arg:id ...)
        #:fmt format-e:expr)
     (with-syntax ([inst:name (format-id #'name "inst:~a" #'name)])
       (syntax/loc stx
         (struct inst:name (arg ...)
           #:methods gen:ll-formatable
           [(define (ll-format v)
              (match-define (inst:name arg ...) v)
              format-e)])))]))

;; Terminators
(define-ll-inst (ret-val v)
  #:fmt (list "ret" (ll-ty v) v))
(define-ll-inst (ret-void)
  #:fmt "ret void")
(define-ll-inst (br cond-v label-true label-false)
  #:fmt (list "br" "i1" cond-v "," "label" label-true "," "label" label-false))
(define-ll-inst (jump label-dest)
  #:fmt (list "br" "label" label-dest))
(define-ll-inst (switch switch-v label-default cases)
  #:fmt (list "switch"
              (ll-ty switch-v) switch-v ","
              "label" label-default
              "["
              (fmt:indent
               (for/list ([c (in-list cases)])
                 (match-define (cons val label) c)
                 (list (ll-ty val) val "," "label" label)))
              "]"))
;; /Terminators

(define-ll-inst (define id val)
  #:fmt (list id "=" val))

(define-syntax (define-ll-expr stx)
  (syntax-parse stx
    [(_ (name:id arg:id ...)
        #:ty type-e:expr
        #:fmt format-e:expr)
     (with-syntax ([expr:name (format-id #'name "expr:~a" #'name)])
       (syntax/loc stx
         (struct expr:name (arg ...)
           #:methods gen:ll-formatable
           [(define (ll-format v)
              (match-define (expr:name arg ...) v)
              format-e)]
           #:methods gen:ll-typeable
           [(define (ll-ty v)
              (match-define (expr:name arg ...) v)
              type-e)])))]))

;; Variables
(define-ll-expr (local ty id)
  #:ty ty
  #:fmt id)
(define-ll-expr (global ty id)
  #:ty (ty:ptr ty)
  #:fmt id)

;; Binary
(define-ll-expr (mul res-ty op1 op2)
  #:ty res-ty
  #:fmt (list "mul" res-ty op1 "," op2))

;; Memory
(define-ll-expr (get-element-ptr ptr-val path)
  #:ty (error 'get-element-ptr)
  #:fmt (list "getelementptr" (ll-ty ptr-val) ptr-val ","
              (fmt:between
               (for/list ([p (in-list path)])
                 (list (ll-ty p) p))
               ",")))
;; /Memory

;; Misc
(define-ll-expr (call-external res-ty fn-ptr-val args)
  #:ty res-ty
  #:fmt (list "call" res-ty
              (vector
               fn-ptr-val
               "("
               (fmt:between
                (for/list ([p (in-list args)])
                  (cons (ll-ty p) p))
                ",")
               ")")))
;; /Misc

(define-syntax (define-ll-top stx)
  (syntax-parse stx
    [(_ (name:id arg:id ...)
        #:fmt format-e:expr)
     (with-syntax ([top:name (format-id #'name "top:~a" #'name)])
       (syntax/loc stx
         (struct top:name (arg ...)
           #:methods gen:ll-formatable
           [(define (ll-format v)
              (match-define (top:name arg ...) v)
              format-e)])))]))

(define-ll-top (variable name val)
  ;; xxx private unnamed_addr constant
  #:fmt (list name "=" (ll-ty val) val))
(define-ll-top (declare-external ret-ty name param-tys)
  #:fmt (list "declare" ret-ty (vector name "(" (fmt:between param-tys ",") ")")))
(define-ll-top (define-external ret-ty name params body)
  #:fmt (list "define" ret-ty
              (vector name "(" (fmt:between params ",") ")")
              ;; xxx body to cfg
              "{"
              (fmt:indent body)
              "}"))

(struct ll-module (l)
  #:methods gen:ll-formatable
  [(define (ll-format v)
     (match-define (ll-module l) v)
     (fmt:between l (fmt:newline)))])

(module+ test
  (ll-out
   (ll-module
    (list
     (top:variable "@.str" (val:string "Hello World!\n\0"))
     (top:declare-external (ty:i32) "@puts" (list (ty:ptr (ty:i8))))

     (top:define-external
      (ty:i32) "@main" (list)
      (list
       (inst:define "%cast210"
                    (expr:get-element-ptr
                     (expr:global (ty:array 13 (ty:i8)) "@.str")
                     (list (val:i64 0)
                           (val:i64 0))))
       (inst:define "%putscode"
                    (expr:call-external
                     (ty:i32) "@puts"
                     (list (expr:local (ty:ptr (ty:i8)) "%cast210"))))
       (inst:ret-val (val:i32 0))))

     (top:define-external
      (ty:i32) "@square_unsigned" (list (cons (ty:i32) "%a"))
      (list
       (inst:define "%1" (expr:mul (ty:i32) "%a" "%a"))
       (inst:ret-val (expr:local (ty:i32) "%1"))))

     (top:define-external
      (ty:vector 4 (ty:i32)) "@multiply_four"
      (list (cons (ty:vector 4 (ty:i32)) "%a")
            (cons (ty:vector 4 (ty:i32)) "%b"))
      (list (inst:define "%1" (expr:mul (ty:vector 4 (ty:i32)) "%a" "%b"))
            (inst:ret-val (expr:local (ty:vector 4 (ty:i32)) "%1"))))))))

(module+ notes
  '[Instructions
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
     (store val ptr)]
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
     ;; xxx internal means fastcc
     (call:internal fnptr arg ...)]]

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
