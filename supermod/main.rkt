#lang racket/base
(require racket/match
         racket/set
         "stdlib.rkt")

;; mod->code represents the zo cache on the filesystem
(struct runtime (mod->code phase0:mod->val phase1:mod->val)
        #:transparent)
(define (make-runtime-system)
  (runtime (hash) (hash) (hash)))

;; xxx templates are bit wrong relative to racket, because they aren't
;; on-demand, in other words, this doesn't have the concept of
;; "available" versus "instantiated"

;; xxx demonstrate for-template ("recursive" or "self")
;; xxx add syntax-local-value
;; xxx add let-syntax
;; xxx implement hygeine, add lexical context to e in invoke-expander

(define (read-mod rts mod)
  (vector-ref
   (hash-ref
    STDLIB mod
    (λ ()
      (error 'read-mod "Cannot find source of ~e" mod)))
   0))

(define (compile-mod rts req-path mod)
  (cond
    [(hash-has-key? (runtime-mod->code rts) mod)
     rts]
    [(set-member? req-path mod)
     (error 'compile-mod "Recursive compile detected for ~e" mod)]
    [else
     (define mod-src (read-mod rts mod))
     (define fresh-rts
       (struct-copy runtime (make-runtime-system)
                    [mod->code
                     (runtime-mod->code rts)]))
     (define-values (fresh-rts-p mod-code)
       (expand-mod-src fresh-rts (set-add req-path mod) (hash) mod-src))
     (struct-copy
      runtime rts
      [mod->code
       (hash-set (runtime-mod->code fresh-rts-p)
                 mod mod-code)])]))

(define (expand-mod-src rts req-path exp-env mod-src)
  (match mod-src
    [`(#%return ,e)
     (values rts `(#%return ,(expand-expr exp-env e)))]
    [`(#%link ,mod ,e)
     (define-values (rts-p mc) (expand-mod-src rts req-path exp-env e))
     (values rts-p `(#%link ,mod ,mc))]
    [`(#%template ,mod ,e)
     (define-values (rts-p mc) (expand-mod-src rts req-path exp-env e))
     (values rts-p `(#%template ,mod ,mc))]
    [`(#%transform ,mod ,e)
     (define phase1-rts
       (struct-copy
        runtime (make-runtime-system)
        ;; copy zos
        [mod->code (runtime-mod->code rts)]
        ;; shift phase1 down to phase0
        [phase0:mod->val (runtime-phase1:mod->val rts)]))
     (define-values (phase1-rts-p mod-res)
       (interp-mod phase1-rts req-path mod))
     (define rts-p
       (struct-copy
        runtime rts
        ;; copy zos
        [mod->code (runtime-mod->code phase1-rts-p)]
        ;; shift phase0 up to phase1
        [phase1:mod->val (runtime-phase0:mod->val phase1-rts-p)]))
     (match-define (MOD-RESULT mod-temps mod-val) mod-res)
     (define exp-env-p
       (hash-set exp-env mod mod-val))
     (expand-mod-src
      rts-p req-path exp-env-p
      (for/fold ([e e])
          ([mod (in-set mod-temps)])
        `(#%link ,mod ,e)))]
    [`(#%invoke ,x . ,_)
     (expand-mod-src
      rts req-path exp-env
      (invoke-expander exp-env x mod-src))]
    [_
     (error 'expand-mod-src "unexpected term ~e" mod-src)]))

(define (expand-expr exp-env e)
  (match e
    [(or `(#%top ,_)
         `(#%local ,_)
         (? number?)
         (? string?)
         (? boolean?)
         '#%null
         `',_)
     e]
    [`(#%lambda ,x ,e)
     `(#%lambda ,x ,(expand-expr exp-env e))]
    [`(#%invoke ,x . ,_)
     (expand-expr exp-env
                  (invoke-expander exp-env x e))]
    [`(,form . ,args)
     `(,form . ,(map (λ (e) (expand-expr exp-env e)) args))]
    [_
     (error 'expand-expr "unexpected term ~e" e)]))

(define (invoke-expander exp-env x e)
  (define f
    (hash-ref exp-env x
              (λ () (error 'invoke-expander "untransform'd identifier ~e" x))))
  (define e-p
    (apply-closure f e))
  e-p)

(define (interp-mod rts req-path mod)
  (cond
    [(set-member? req-path mod)
     (error 'interp-mod "Recursive require detected for ~e" mod)]
    [else
     (define rts-p (compile-mod rts req-path mod))
     (define mod-code (hash-ref (runtime-mod->code rts-p) mod))
     (define mod->val (runtime-phase0:mod->val rts-p))
     (cond
       [(hash-has-key? mod->val mod)
        (values rts-p (hash-ref mod->val mod))]
       [else
        (define-values (rts-pp mod-val)
          (interp-mod-code rts-p (set-add req-path mod) (hash) mod-code))
        (values
         (struct-copy
          runtime rts-pp
          [phase0:mod->val
           (hash-set (runtime-phase0:mod->val rts-pp) mod mod-val)])
         mod-val)])]))

(struct MOD-RESULT (templates val) #:transparent)
(define (interp-mod-code rts req-path top-env mc)
  (match mc
    [`(#%return ,e)
     (values rts (MOD-RESULT (set) (interp-expr top-env (hasheq) e)))]
    [`(#%link ,mod ,mc)
     (define-values (rts-p mod-res)
       (interp-mod rts req-path mod))
     (match-define (MOD-RESULT mod-temps mod-val) mod-res)
     (define top-env-p
       (hash-set top-env mod mod-val))
     (interp-mod-code
      rts-p req-path top-env-p
      (for/fold ([mc mc])
          ([mod (in-set mod-temps)])
        `(#%template ,mod ,mc)))]
    [`(#%template ,mod ,mc)
     (define-values (rts-p mr)
       (interp-mod-code rts req-path top-env mc))
     (values rts-p
             (struct-copy MOD-RESULT mr
                          [templates
                           (set-add (MOD-RESULT-templates mr) mod)]))]
    [_
     (error 'interp-mod-code "unexpected term ~e" mc)]))

(struct CLOSURE (arg body top-env env))
(define (apply-closure fv av)
  (match fv
    [(CLOSURE arg body clo-top-env clo-env)
     (interp-expr
      clo-top-env
      (hash-set clo-env arg av)
      body)]))

(define PRIMS
  (hasheq
   '#%cons cons
   '#%eq? eq?
   '#%car car
   '#%cdr cdr
   '#%box box
   '#%unbox unbox
   '#%set-box! (λ (b nv) (set-box! b nv) nv)))
(define (PRIMITIVE? x)
  (hash-has-key? PRIMS x))
(define (interp-expr top-env env e)
  (match e
    [(or (? number? n)
         (? string? n)
         (? boolean? n))
     n]
    ['#%null
     null]
    [`',s
     s]
    [(cons (? PRIMITIVE? prim) args)
     (apply (hash-ref PRIMS prim) 
            (map (λ (e) (interp-expr top-env env e))
                 args))]
    [`(#%if ,c ,t ,f)
     (interp-expr top-env env (if (interp-expr top-env env c) t f))]
    [`(#%lambda ,arg ,body)
     (CLOSURE arg body top-env env)]
    [`(#%app ,f ,e)
     (apply-closure
      (interp-expr top-env env f)
      (interp-expr top-env env e))]
    [`(#%local ,x)
     (hash-ref env x (λ () (error 'interp-expr "unbound local identifier ~e" x)))]
    [`(#%top ,x)
     (hash-ref top-env x (λ () (error 'interp-expr "unrequired top identifier ~e" x)))]
    [_
     (error 'interp-expr "unexpected term ~e" e)]))

(define (string->mod name)
  (local-require racket/string)
  (map string->symbol (string-split name "/")))

(define (supermod name)
  (interp-mod
   (make-runtime-system)
   (set)
   (string->mod name)))

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "supermod"
   #:args (name)
   (supermod name)))

(module+ test
  (require rackunit
           racket/pretty)

  (define (check-mod-mc*v rts mod mc*v)
    (match-define (vector mc exp-val) mc*v)

    (define-values (rts-p mr)
      (interp-mod rts (set) mod))
    (define act-val (MOD-RESULT-val mr))
    (test-case
     (format "~a: ~e\n~v" mod mc rts-p)
     (match exp-val
       ['CLOSURE?
        (check-pred CLOSURE? act-val)]
       ['BOX?
        (check-pred box? act-val)]
       [_
        (check-equal? act-val exp-val)]))
    rts-p)

  (for ([(mod mc*v) (in-hash STDLIB)])
    (define rts (make-runtime-system))
    (define rts-p (check-mod-mc*v rts mod mc*v))
    (test-case
     (format "~a: second run" mod)
     (for ([(mod mc*v) (in-hash STDLIB)])
       (check-mod-mc*v rts-p mod mc*v)))))
