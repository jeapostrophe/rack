#lang racket/base
(require racket/match
         "ast.rkt"
         racket/contract
         rackunit
         racket-llvm/private/ffi/all
         racket-llvm/private/ffi/ctypes)

(define (verify p)
  (verify-prog p))
(define (verify-prog p)
  (match p
    [(prog d)
     (define genv
       (for/fold ([genv (hasheq)])
           ([d (in-list d)])
         (define-values (id ty) (type-defn #f d))
         (hash-set genv id ty)))
     (for-each (λ (d) (type-defn genv d))
               d)]
    [_
     (error 'verify-prog "can't check ~e" p)]))

(define (defn:fun-type d)
  (match-define (defn:fun ret _ args) d)
  (type:fun (for/vector ([a (in-vector args)])
              (vector-ref a 1))
            ret))

(define (type-defn genv d)
  (match d
    [(defn:fun:extern ret id args)
     (values id (defn:fun-type d))]
    [(defn:fun:local ret id args body)
     (when genv
       (define env
         (for/hasheq
          ([a (in-vector args)])
          (values (vector-ref a 0) (vector-ref a 1))))
       (define actual-ret (type-stmt genv env body))
       (check-equal? actual-ret ret))
     (values id (defn:fun-type d))]
    [_
     (error 'type-defn "can't check ~e" d)]))

(define (type-stmt genv env s)
  (match s
    [(stmt:ret e)
     (type-expr genv env e)]
    [(stmt:let id val body)
     (define t (type-expr genv env val))
     (define env-p (hash-set env id t))
     (type-stmt genv env-p body)]
    [_
     (error 'type-stmt "can't check ~e" s)]))

(define (type-expr genv env e)
  (match e
    [(expr:call:ext fun args)
     (match-define (type:fun eargs-t rt) (type-expr genv env fun))
     (for ([a (in-vector args)]
           [et (in-vector eargs-t)])
       (check-equal? (type-expr genv env a) et))
     rt]
    [(expr:let id val body)
     (define ty (type-expr genv env val))
     (type-expr genv (hash-set env id ty) body)]
    [(expr:int lhs rhs)
     (define lt (type-expr genv env lhs))
     (define rt (type-expr genv env rhs))
     (check-equal? lt rt)
     (check-pred type:atom:int? lt)
     lt]
    [(expr:float lhs rhs)
     (define lt (type-expr genv env lhs))
     (define rt (type-expr genv env rhs))
     (check-equal? lt rt)
     (check-pred type:atom:float? lt)
     lt]
    [(expr:convert from-e to-ty)
     (define from-ty (type-expr genv env from-e))
     (match e
       [(? expr:convert:i2i?)
        (check-pred type:atom:int? from-ty)
        (check-pred type:atom:int? to-ty)]
       [(? expr:convert:f2f?)
        (check-pred type:atom:float? from-ty)
        (check-pred type:atom:float? to-ty)]
       [(? expr:convert:f2i?)
        (check-pred type:atom:float? from-ty)
        (check-pred type:atom:int? to-ty)]
       [(? expr:convert:i2f?)
        (check-pred type:atom:int? from-ty)
        (check-pred type:atom:float? to-ty)])
     (match e
       [(? expr:convert:i2i:trunc?)
        (check-true (< (type:atom:int-w to-ty)
                       (type:atom:int-w from-ty)))]
       [(or (? expr:convert:i2i:zext?)
            (? expr:convert:i2i:sext?))
        (check-true (< (type:atom:int-w from-ty)
                       (type:atom:int-w to-ty)))]
       [(? expr:convert:f2f:trunc?)
        (check-true (< (type:atom:float-w to-ty)
                       (type:atom:float-w from-ty)))]
       [(? expr:convert:f2f:ext?)
        (check-true (< (type:atom:float-w from-ty)
                       (type:atom:float-w to-ty)))]
       [_
        (void)])
     to-ty]
    [(expr:global-ref id)
     (hash-ref genv id
               (λ () (error 'type-expr "unknown global variable: ~a" id)))]
    [(expr:local-ref id)
     (hash-ref env id
               (λ () (error 'type-expr "unknown local variable: ~a" id)))]
    [(expr:val:int w _)
     (type:atom:int w)]
    [(expr:val:float w _)
     (type:atom:float w)]
    [_
     (error 'type-expr "can't check ~e" e)]))

;; xxx memoize
(define (type->llvm-type t)
  (match t
    [(type:atom:int w)
     (unsafe:LLVMIntType w)]
    [(type:atom:float 16)
     (unsafe:LLVMHalfType)]
    [(type:atom:float 32)
     (unsafe:LLVMFloatType)]
    [(type:atom:float 64)
     (unsafe:LLVMDoubleType)]
    [(type:atom:float 80)
     (unsafe:LLVMX86FP80Type)]
    [(type:atom:float 128)
     (unsafe:LLVMFP128Type)]
    [(type:fun args ret)
     (unsafe:LLVMFunctionType
      (type->llvm-type ret)
      (for/list ([a (in-vector args)])
        (type->llvm-type a))
      #f)]
    [_
     (error 'type->llvm-type "can't convert ~e" t)]))

(define (compile p)
  (define mod
    (unsafe:LLVMModuleCreateWithName
     (symbol->string (gensym 'anonymous))))

  (define exec-eng
    (unsafe:LLVMCreateExecutionEngineForModule mod))
  (define layout
    (unsafe:LLVMGetExecutionEngineTargetData exec-eng))
  (define fpm
    (unsafe:LLVMCreateFunctionPassManagerForModule mod))

  ;; xxx add more?
  (unsafe:LLVMAddTargetData layout fpm)
  (unsafe:LLVMAddBasicAliasAnalysisPass fpm)
  (unsafe:LLVMAddInstructionCombiningPass fpm)
  (unsafe:LLVMAddReassociatePass fpm)
  (unsafe:LLVMAddGVNPass fpm)
  (unsafe:LLVMAddCFGSimplificationPass fpm)

  (unsafe:LLVMInitializeFunctionPassManager fpm)

  (match p
    [(prog d)
     (define genv
       (for/hasheq 
        ([d (in-list d)])
        (declare-defn mod d)))
     (for ([d (in-list d)])
       (compile-defn genv mod fpm d))
     mod]
    [_
     (error 'compile "can't compile: ~e" p)]))

(define (declare-defn mod d)
  (match d
    [(defn:fun ret fun args)
     (define fun-s (symbol->string fun))
     (define fun-ty (defn:fun-type d))
     (define fun-ty-llvm (type->llvm-type fun-ty))
     (define the-fun (unsafe:LLVMAddFunction mod fun-s fun-ty-llvm))
     (values fun the-fun)]
    [_
     (error 'declare-defn "can't declare: ~e" d)]))

(define (compile-defn genv mod fpm d)
  (match d
    [(defn:fun:extern ret fun args)
     (void)]
    [(defn:fun:local:ext ret fun args body)
     (define the-fun
       (hash-ref genv fun))

     (define fun-env
       (for/fold ([env (hasheq)])
           ([a (in-vector args)]
            [i (in-naturals)])
         (hash-set env (vector-ref a 0) (unsafe:LLVMGetParam the-fun i))))

     (define builder (unsafe:LLVMCreateBuilder))
     (define bb (unsafe:LLVMAppendBasicBlock the-fun "entry"))
     (unsafe:LLVMPositionBuilderAtEnd builder bb)

     (compile-stmt genv builder fun-env body)

     (unsafe:LLVMVerifyFunction the-fun 'LLVMAbortProcessAction)

     (unsafe:LLVMRunFunctionPassManager fpm the-fun)]
    [_
     (error 'compile-defn "can't compile: ~e" d)]))

(define (compile-stmt genv builder env s)
  (match s
    [(stmt:ret e)
     (define v (compile-expr genv builder env e))
     (unsafe:LLVMBuildRet builder v)]
    [(stmt:let id val body)
     (define env-p
       (hash-set env id (compile-expr genv builder env val)))
     (compile-stmt genv builder env-p body)]
    [_
     (error 'compile-stmt "can't compile: ~e" s)]))

(define (compile-expr genv builder env e)
  (match e
    [(expr:call:ext fun args)
     (unsafe:LLVMBuildCall 
      builder
      (compile-expr genv builder env fun)
      (for/list ([a (in-vector args)])
        (compile-expr genv builder env a))
      "call")]
    [(expr:int lhs rhs)
     (define lv (compile-expr genv builder env lhs))
     (define rv (compile-expr genv builder env rhs))
     (define-syntax-rule (matcher e [pred? con] ...)
       (match e
         [(? pred?) (con builder lv rv "int")]
         ...
         [_
          (error 'compile-expr "unknown integer op: ~e" e)]))
     (matcher e
       [expr:int:add? unsafe:LLVMBuildAdd]       
       [expr:int:sub? unsafe:LLVMBuildSub]
       [expr:int:mul? unsafe:LLVMBuildMul]
       [expr:int:udiv? unsafe:LLVMBuildUDiv]
       [expr:int:sdiv? unsafe:LLVMBuildSDiv]
       [expr:int:urem? unsafe:LLVMBuildURem]
       [expr:int:srem? unsafe:LLVMBuildSRem]
       [expr:int:shl? unsafe:LLVMBuildShl]
       [expr:int:lshr? unsafe:LLVMBuildLShr]
       [expr:int:ashr? unsafe:LLVMBuildAShr]
       [expr:int:and? unsafe:LLVMBuildAnd]
       [expr:int:ior? unsafe:LLVMBuildOr]
       [expr:int:xor? unsafe:LLVMBuildXor])]
    [(expr:float lhs rhs)
     (define lv (compile-expr genv builder env lhs))
     (define rv (compile-expr genv builder env rhs))
     (define-syntax-rule (matcher e [pred? con] ...)
       (match e
         [(? pred?) (con builder lv rv "float")]
         ...
         [_
          (error 'compile-expr "unknown float op: ~e" e)]))
     (matcher e
       [expr:float:add? unsafe:LLVMBuildFAdd]
       [expr:float:sub? unsafe:LLVMBuildFSub]
       [expr:float:mul? unsafe:LLVMBuildFMul]
       [expr:float:div? unsafe:LLVMBuildFDiv]
       [expr:float:rem? unsafe:LLVMBuildFRem])]
    [(expr:convert val ty)
     (define cv (compile-expr genv builder env val))
     (define lty (type->llvm-type ty))
     (define-syntax-rule (matcher e [pred? con] ...)
       (match e
         [(? pred?) (con builder cv lty "convert")]
         ...
         [_
          (error 'compile-expr "unknown conversion op: ~e" e)]))
     (matcher e
       [expr:convert:f2f:ext? unsafe:LLVMBuildFPExt]
       [expr:convert:f2f:trunc? unsafe:LLVMBuildFPTrunc]
       [expr:convert:f2i:s? unsafe:LLVMBuildFPToSI]
       [expr:convert:f2i:u? unsafe:LLVMBuildFPToUI]
       [expr:convert:i2f:s? unsafe:LLVMBuildSIToFP]
       [expr:convert:i2f:u? unsafe:LLVMBuildUIToFP]
       [expr:convert:i2i:trunc? unsafe:LLVMBuildTrunc]
       [expr:convert:i2i:sext? unsafe:LLVMBuildSExt]
       [expr:convert:i2i:zext? unsafe:LLVMBuildZExt])]
    [(expr:let id val body)
     (define cv
       (compile-expr genv builder env val))
     (define env-p
       (hash-set env id cv))
     (compile-expr genv builder env-p body)]
    [(expr:global-ref id)
     (hash-ref genv id
               (λ () (error 'compile-expr "unknown global variable: ~a" id)))]
    [(expr:local-ref id)
     (hash-ref env id
               (λ () (error 'compile-expr "unknown local variable: ~a" id)))]
    [(expr:val:int w v)
     (unsafe:LLVMConstInt (type->llvm-type (type:atom:int w)) v #t)]
    [(expr:val:float w v)
     (unsafe:LLVMConstReal (type->llvm-type (type:atom:float w)) v)]
    [_
     (error 'compile-expr "can't compile: ~e" e)]))

(void
 '
 (define (compile^ prog)
   (define (codegen builder env e)
     (match e
       [`(,(and (or '+ '- '* '<) op) ,lhs ,rhs)
        (define L (codegen builder env lhs))
        (define R (codegen builder env rhs))
        (match op
          ['<
           (unsafe:LLVMBuildUIToFP
            builder
            (unsafe:LLVMBuildFCmp builder 'LLVMRealULT L R "cmptmp")
            (unsafe:LLVMDoubleType)
            "booltmp")]
          [_
           (error 'compile "invalid binary operator: ~e" op)])]
       [`(if ,cond ,true, false)
        (define C (codegen builder env cond))

        (define CmpV
          (unsafe:LLVMBuildFCmp
           builder 'LLVMRealONE
           C
           (unsafe:LLVMConstReal (unsafe:LLVMDoubleType) 0) "ifcond"))

        (define cur-fun
          (unsafe:LLVMGetBasicBlockParent
           (unsafe:LLVMGetInsertBlock builder)))

        (define then-bb (unsafe:LLVMAppendBasicBlock cur-fun "then"))
        (define else-bb (unsafe:LLVMAppendBasicBlock cur-fun "else"))
        (define merge-bb (unsafe:LLVMAppendBasicBlock cur-fun "ifcont"))

        (unsafe:LLVMBuildCondBr builder CmpV then-bb else-bb)

        (unsafe:LLVMPositionBuilderAtEnd builder then-bb)
        (define T (codegen builder env true))
        (unsafe:LLVMBuildBr builder merge-bb)
        (define new-then-bb (unsafe:LLVMGetInsertBlock builder))

        (unsafe:LLVMPositionBuilderAtEnd builder else-bb)
        (define F (codegen builder env false))
        (unsafe:LLVMBuildBr builder merge-bb)
        (define new-else-bb (unsafe:LLVMGetInsertBlock builder))

        (unsafe:LLVMPositionBuilderAtEnd builder merge-bb)
        (define PN (unsafe:LLVMBuildPhi builder (unsafe:LLVMDoubleType) "iftmp"))
        (unsafe:LLVMAddIncoming
         PN
         (list           T           F)
         (list new-then-bb new-else-bb))

        PN]))))

(define (run mod)
  (define main-f
    (unsafe:LLVMGetNamedFunction mod "main"))
  ;; xxx should this use the same one?
  (define exec-eng
    (unsafe:LLVMCreateExecutionEngineForModule mod))
  (define res (unsafe:LLVMRunFunction exec-eng main-f (list)))
  (unsafe:LLVMGenericValueToInt res #t))

(define (dump mod)
  (unsafe:LLVMDumpModule mod))

(define (emit mod path)
  (define r (unsafe:LLVMWriteBitcodeToFile mod path))
  (unless (zero? r)
    (error 'emit "error while writing")))

(provide
 (contract-out
  [verify
   (-> prog?
       void?)]
  [compile
   (-> prog?
       unsafe:llvm-module-ref?)]
  [run
   (-> unsafe:llvm-module-ref?
       exact-nonnegative-integer?)]
  [emit
   (-> unsafe:llvm-module-ref? path-string?
       void?)]
  [dump
   (-> unsafe:llvm-module-ref?
       void?)]))
