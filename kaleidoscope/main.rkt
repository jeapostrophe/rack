#lang racket/base
(require racket/match
         racket-llvm/private/ffi/all)

(define (compile prog)
  (define mod
    (unsafe:LLVMModuleCreateWithName
     (symbol->string (gensym 'anonymous))))

  (define exec-eng
    (unsafe:LLVMCreateExecutionEngineForModule mod))
  (define fpm
    (unsafe:LLVMCreateFunctionPassManagerForModule mod))

  (unsafe:LLVMAddTargetData
   (unsafe:LLVMGetExecutionEngineTargetData exec-eng)
   fpm)
  (unsafe:LLVMAddBasicAliasAnalysisPass fpm)
  (unsafe:LLVMAddInstructionCombiningPass fpm)
  (unsafe:LLVMAddReassociatePass fpm)
  (unsafe:LLVMAddGVNPass fpm)
  (unsafe:LLVMAddCFGSimplificationPass fpm)

  (unsafe:LLVMInitializeFunctionPassManager fpm)

  (define (codegen builder env e)
    ;; (eprintf "\t~e\n" e)
    (match e
      [(? number? n)
       (unsafe:LLVMConstReal (unsafe:LLVMDoubleType) n)]
      [(? symbol? v)
       (hash-ref env v)]
      [`(,(and (or '+ '- '* '<) op) ,lhs ,rhs)
       (define L (codegen builder env lhs))
       (define R (codegen builder env rhs))
       (match op
         ['+
          (unsafe:LLVMBuildFAdd builder L R "addtmp")]
         ['-
          (unsafe:LLVMBuildFSub builder L R "subtmp")]
         ['*
          (unsafe:LLVMBuildFMul builder L R "multmp")]
         ['<
          (unsafe:LLVMBuildUIToFP
           builder
           (unsafe:LLVMBuildFCmp builder 'LLVMRealULT L R "cmptmp")
           (unsafe:LLVMDoubleType)
           "booltmp")]
         [_
          (error 'compile "invalid binary operator: ~e" op)])]
      [`(if ,cond ,true, false)
       (error 'compile "xxx if")]
      [(list-rest fun args)
       (define callee
         (unsafe:LLVMGetNamedFunction mod (symbol->string fun)))
       (unless callee
         (error 'compile "Undefined function: ~e" fun))
       (unless (= (length args) (unsafe:LLVMCountParams callee))
         (error 'compile
                "Wrong number of arguments on ~e, ~e vs ~e"
                fun
                (length args)
                (unsafe:LLVMCountParams callee)))
       (unsafe:LLVMBuildCall
        builder callee
        (for/list ([a (in-list args)])
          (codegen builder env a))
        "calltmp")]
      [_
       (error 'compile "invalid expression: ~e" e)]))

  (define (make-fun-ty count)
    (unsafe:LLVMFunctionType
     (unsafe:LLVMDoubleType)
     (for/list ([a (in-range count)])
       (unsafe:LLVMDoubleType))
     #f))

  (for ([def (in-list prog)])
    (match def
      [`(define-extern ,fun ,arg-count)
       (define fun-s (symbol->string fun))
       (define fun-ty (make-fun-ty arg-count))
       (unsafe:LLVMAddFunction mod fun-s fun-ty)]
      [`(define (,fun . ,args) ,body)
       (define fun-s (symbol->string fun))
       (define fun-ty (make-fun-ty (length args)))
       (define maybe-fun
         (unsafe:LLVMGetNamedFunction mod fun-s))
       (define the-fun
         (if (or
              (regexp-match #rx"null" (format "~a" maybe-fun))
              ;; xxx this crashes?
              (unsafe:LLVMIsNull maybe-fun))
           (unsafe:LLVMAddFunction mod fun-s fun-ty)
           maybe-fun))

       (define fun-env
         (for/fold ([env (hasheq)])
             ([a (in-list args)]
              [i (in-naturals)])
           (hash-set env a (unsafe:LLVMGetParam the-fun i))))

       (define builder (unsafe:LLVMCreateBuilder))
       (define bb (unsafe:LLVMAppendBasicBlock the-fun "entry"))
       (unsafe:LLVMPositionBuilderAtEnd builder bb)
       (define return-val (codegen builder fun-env body))
       (unsafe:LLVMBuildRet builder return-val)

       (unsafe:LLVMVerifyFunction the-fun 'LLVMAbortProcessAction)

       (unsafe:LLVMRunFunctionPassManager fpm the-fun)]
      [_
       (error 'compile "invalid definition: ~e" def)]))

  (unsafe:LLVMDumpModule mod)

  (define main-f
    (unsafe:LLVMGetNamedFunction mod "main"))

  (λ ()
    (unsafe:LLVMRunFunction exec-eng main-f (list))))

(module+ test
  (require rackunit)
  (define-syntax-rule (define&check id prog ...)
    (let ()
      (define id (list 'prog ...))
      (eprintf "Compiling ~a\n" 'id)
      (define main-f (compile id))
      (printf " Answer: ~a\n"
              (unsafe:LLVMGenericValueToFloat
               (unsafe:LLVMDoubleType)
               (main-f)))))

  (define&check p:num
    (define (main)
      5))
  (define&check p:add
    (define (main)
      (+ 1 2)))
  (define&check p:add45
    (define (main)
      (+ 4 5)))
  (define&check p:sub
    (define (main)
      (- 1 2)))
  (define&check p:mul
    (define (main)
      (* 1 2)))
  (define&check p:cmp
    (define (main)
      (< 1 2)))
  (define&check p:fun
    (define (add1 x)
      (+ x 1))
    (define (main)
      (add1 5)))
  (check-exn
   exn:fail?
   (λ ()
     (define&check p:fun-err
       (define (add1 x)
         (+ x 1))
       (define (main)
         (add1 5 6)))))
  (define&check p:foo
    (define (foo a b)
      (+ (+ (* a a)
            (* 2 (* a b)))
         (* b b)))
    (define (bar a)
      (+ (foo a 4.0)
         (bar 31337)))
    (define (main)
      (foo 31337 2)))
  (define&check p:extern
    (define-extern sin 1)
    (define-extern cos 1)
    (define-extern atan2 2)
    (define (main)
      (atan2 (sin .4) (cos 42))))
  (define&check p:fold
    (define (test x)
      (* (+ 1 (+ 2 x))
         (+ x (+ 1 2))))
    (define (main)
      (test 3)))
  (define&check p:testfunc
    (define (testfunc x y)
      (+ x (* y 2)))
    (define (main)
      (testfunc 4 10)))
  (define&check p:fib
    (define (fib x)
      (if (< x 3)
        1
        (+ (fib (- x 1))
           (fib (- x 2)))))
    (define (main)
      (fib 40))))
