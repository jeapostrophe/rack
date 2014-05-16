#lang racket/base
(require racket/match
         racket/set
         racket/pretty
         "stdlib.rkt")

(struct runtime (mod->code phase->mod->val) #:transparent)
(define (make-runtime-system)
  (runtime (hash) (hasheq)))

;; xxx add mutation and separate compilation
;; xxx demonstrate for-template ("recursive" or "self")
;; xxx add syntax-local-value

(define (read-mod rts mod)
  (vector-ref
   (hash-ref
    STDLIB mod
    (λ ()
      (error 'read-mod "Cannot find source of ~e" mod)))
   0))

(define (compile-mod rts req-path phase mod)
  (cond
    [(hash-has-key? (runtime-mod->code rts) mod)
     rts]
    [(set-member? req-path mod)
     (error 'compile-mod "Recursive compile detected for ~e" mod)]
    [else
     (define mod-src (read-mod rts mod))
     ;; xxx clean out other phases
     (define-values (rts-p mod-code)
       (expand-mod-src rts (set-add req-path mod) phase (hash) mod-src))
     (struct-copy
      runtime rts-p
      [mod->code
       (hash-set (runtime-mod->code rts-p)
                 mod mod-code)])]))

(define (expand-mod-src rts req-path phase exp-env mod-src)
  (match mod-src
    [`(#%return ,e)
     (values rts `(#%return ,(expand-expr exp-env e)))]
    [`(#%link ,mod ,e)
     (define-values (rts-p mc) (expand-mod-src rts req-path phase exp-env e))
     (values rts-p `(#%link ,mod ,mc))]
    [`(#%template ,mod ,e)
     (define-values (rts-p mc) (expand-mod-src rts req-path phase exp-env e))
     (values rts-p `(#%template ,mod ,mc))]
    [`(#%transform ,mod ,e)
     (define-values (rts-p mod-res)
       (interp-mod rts req-path (add1 phase) mod))
     (match-define (MOD-RESULT mod-temps mod-val) mod-res)
     (define exp-env-p
       (hash-set exp-env mod mod-val))
     (define-values (rts-pp mc)
       (expand-mod-src 
        rts-p req-path phase exp-env-p 
        (for/fold ([e e])
            ([mod (in-set mod-temps)])
          `(#%link ,mod ,e))))
     (values rts-pp mc)]
    [`(#%invoke ,x . ,_)
     (expand-mod-src
      rts req-path phase exp-env
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
    [`(#%app ,f ,e)
     `(#%app ,(expand-expr exp-env f)
             ,(expand-expr exp-env e))]
    [`(#%cons ,f ,e)
     `(#%cons ,(expand-expr exp-env f)
              ,(expand-expr exp-env e))]
    [`(#%eq? ,f ,e)
     `(#%eq? ,(expand-expr exp-env f)
             ,(expand-expr exp-env e))]
    [`(#%if ,c ,t ,f)
     `(#%if ,(expand-expr exp-env c)
            ,(expand-expr exp-env t)
            ,(expand-expr exp-env f))]
    [`(#%car ,e)
     `(#%car ,(expand-expr exp-env e))]
    [`(#%cdr ,e)
     `(#%cdr ,(expand-expr exp-env e))]
    [`(#%lambda ,x ,e)
     `(#%lambda ,x ,(expand-expr exp-env e))]
    [`(#%invoke ,x . ,_)
     (expand-expr exp-env
                  (invoke-expander exp-env x e))]
    [_
     (error 'expand-expr "unexpected term ~e" e)]))

;; xxx implement hygeine, add lexical context to e, etc
(define (invoke-expander exp-env x e)
  (define f
    (hash-ref exp-env x
              (λ () (error 'invoke-expander "untransform'd identifier ~e" x))))
  (define e-p
    (apply-closure f e))
  e-p)

(define (interp-mod rts req-path phase mod)
  (cond
    [(set-member? req-path mod)
     (error 'interp-mod "Recursive require detected for ~e" mod)]
    [else
     (define rts-p (compile-mod rts req-path phase mod))
     (define mod-code (hash-ref (runtime-mod->code rts-p) mod))
     (pretty-print (vector mod mod-code))
     (define mod->val (hash-ref (runtime-phase->mod->val rts-p) phase (hash)))
     (cond
       [(hash-has-key? mod->val mod)
        (values rts-p (hash-ref mod->val mod))]
       [else
        (define-values (rts-pp mod-val)
          (interp-mod-code rts-p (set-add req-path mod) phase (hash) mod-code))
        (values
         (struct-copy
          runtime rts-pp
          [phase->mod->val
           (hash-update (runtime-phase->mod->val rts-pp)
                        phase
                        (λ (old)
                          (hash-set old mod mod-val))
                        (hash))])
         mod-val)])]))

(struct MOD-RESULT (templates val) #:transparent)
(define (interp-mod-code rts req-path phase top-env mc)
  (match mc
    [`(#%return ,e)
     (values rts (MOD-RESULT (set) (interp-expr top-env (hasheq) e)))]
    [`(#%link ,mod ,mc)
     (define-values (rts-p mod-val)
       (interp-mod rts req-path phase mod))
     ;; xxx take its templates?g
     (define top-env-p
       (hash-set top-env mod (MOD-RESULT-val mod-val)))
     (interp-mod-code rts-p req-path phase top-env-p mc)]
    [`(#%template ,mod ,mc)
     (define-values (rts-p mr)
       (interp-mod-code rts req-path phase top-env mc))
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
    [`(#%cons ,a ,d)
     (cons (interp-expr top-env env a)
           (interp-expr top-env env d))]
    [`(#%eq? ,a ,d)
     (eq? (interp-expr top-env env a)
          (interp-expr top-env env d))]
    [`(#%car ,e)
     (car (interp-expr top-env env e))]
    [`(#%cdr ,e)
     (cdr (interp-expr top-env env e))]
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
   0
   (string->mod name)))

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "supermod"
   #:args (name)
   (supermod name)))

(module+ test
  (require rackunit)
  (define final-rts
    (for/fold ([rts (make-runtime-system)])
        ([(mod mc*v) (in-hash STDLIB)])
      (define-values (rts-p mr) (interp-mod rts (set) 0 mod))
      (define act-val (MOD-RESULT-val mr))
      (match-define (vector mc exp-val) mc*v)
      (test-case (format "~a: ~e" mod mc)
                 (match exp-val
                   ['CLOSURE?
                    (check-pred CLOSURE? act-val)]
                   [_
                    (check-equal? act-val exp-val)]))
      rts-p))
  (require racket/pretty)
  (pretty-print final-rts))
