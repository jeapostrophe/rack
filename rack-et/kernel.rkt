#lang racket/base
(require racket/match
         racket/list)

(define-syntax-rule (define-prim* i ...)
  (define prims
    (make-hasheq
     (list (cons 'i #f) ...))))

(define-prim*
  ;; values
  call-with-values
  values
  ;; conts
  call-with-escape-continuation
  call-with-current-continuation
  call-with-continuation-barrier
  call-with-continuation-prompt
  abort-current-continuation
  continuation-prompt-available?
  continuation-marks
  dynamic-wind
  ;; data
  make-vector
  vector-ref
  vector-set!
  make-bytes
  bytes-ref
  bytes-set!
  make-seal
  seal-prop-ref
  ;; custodian (maybe not have this, since no external resources?)
  make-custodian
  custodian-shutdown-all
  custodian-managed-list
  make-custodian-box
  custodian-box-value
  custodian-require-memory
  custodian-limit-memory
  ;; gc
  collect-garbage
  make-ephemeron
  ephemeron-value
  ;; math
  ;; XXX?
  ;; modules
  dynamic-require
  ;; XXX which namespace stuff?
  ;; system
  system-type
  ;; functions
  apply
  ;; ffi
  ffi-lib
  ffi-obj
  ffi-call
  ffi-callback
  )

(define-values (prop:sealed-props sealed-props? sealed-props-ref)
  (make-struct-type-property 'sealed-props))
(define (prim:make-seal props)
  (struct sealed (v)
          #:property prop:sealed-props props)

  (values sealed
          sealed?
          sealed-v))
(define (prim:seal-prop-ref v p dv)
  (let loop ([ps (sealed-props-ref v)])
    (match ps
      [#f
       dv]
      [(vector k v l r)
       (cond
         [(= k p)
          v]
         [(< p k)
          (loop l)]
         [else
          (loop r)])])))


;; Code

(struct rack:expr
        ())
(struct rack:expr:lambda rack:expr
        (params rest-param body))
(struct rack:expr:if0 rack:expr
        (test zero nonzero))
(struct rack:expr:let-values rack:expr
        (ids vals body))
(struct rack:expr:quote rack:expr
        (datum))
(struct rack:expr:wcm rack:expr
        (key val body))
(struct rack:expr:app rack:expr
        (fun args))
(struct rack:expr:ref rack:expr
        (sym))

;; Values

(struct rack:val
        ())
(struct rack:val:clo rack:val
        (env lam))
(struct rack:val:prim rack:val
        (prim))

;; Evaluations

(struct rack:evalctxt
        (env marks))

(struct rack:eval
        (vals ctxt e))
(struct rack:eval:lambda rack:eval
        ())
(struct rack:eval:if0:zero rack:eval
        (test-eval zero-eval))
(struct rack:eval:if0:nonzero rack:eval
        (test-eval nonzero-eval))
(struct rack:eval:let-values rack:eval
        (vals-eval body-eval))
(struct rack:eval:quote rack:eval
        ())
(struct rack:eval:wcm rack:eval
        (k-eval v-eval body-eval))
(struct rack:eval:app:clo rack:eval
        (fun-eval arg-evals body-eval))
(struct rack:eval:app:prim rack:eval
        (fun-eval arg-evals))
(struct rack:eval:ref rack:eval
        ())

(define (interp ctxt e)
  (match-define (rack:evalctxt env marks) ctxt)
  (define fresh-mark-frame
    (hash))
  (define fresh-ctxt
    (rack:evalctxt env (cons fresh-mark-frame marks)))
  (match e
    [(rack:expr:lambda _ _ _)
     ;; xxx shrink env
     (rack:eval:lambda
      (vector (rack:val:clo (rack:evalctxt-env ctxt) e))
      ctxt e)]
    [(rack:expr:if0 test zero nonzero)
     (define test-eval
       (interp fresh-ctxt test))
     (define-values
       (make-eval next-e)
       ;; xxx fix this
       (if (zero? (vector-ref (rack:eval-vals test-eval) 0))
         (values rack:eval:if0:zero zero)
         (values rack:eval:if0:nonzero nonzero)))
     (define next-eval
       (interp ctxt next-e))
     (make-eval
      (rack:eval-vals next-eval)
      ctxt e
      test-eval
      next-eval)]
    [(rack:expr:let-values ids vals body)
     (define vals-eval
       (interp fresh-ctxt vals))
     (define env+
       (for/fold ([env+ env])
           ([i (in-vector ids)]
            [v (in-vector vals)])
         (hash-set env+ i v)))
     (define body-eval
       (interp (rack:evalctxt env+ marks) body))

     (rack:eval:let-values
      (rack:eval-vals body-eval)
      ctxt e
      vals-eval
      body-eval)]
    [(rack:expr:quote d)
     (rack:eval:quote d ctxt e)]
    [(rack:expr:wcm k v body)
     (define k-eval
       (interp fresh-ctxt k))
     (define v-eval
       (interp fresh-ctxt v))
     (define mark-frame+
       (hash-set (first marks) k v))
     (define marks+
       (cons mark-frame+ (rest marks)))
     (define ctxt+
       (rack:evalctxt env marks+))
     (define body-eval
       (interp ctxt+ body))
     (rack:eval:wcm
      (rack:eval-vals body-eval)
      ctxt e
      k-eval
      v-eval
      body-eval)]
    [(rack:expr:app fun args)
     (define fun-eval
       (interp fresh-ctxt fun))
     (define arg-evals
       (for/list ([a (in-list args)])
         (interp fresh-ctxt a)))
     (define arg-vals
       (for/list ([ae (in-list arg-evals)])
         (vector-ref (rack:eval-vals ae) 0)))

     (match (vector-ref (rack:eval-vals fun-eval) 0)
       [(rack:val:clo clo-env (rack:expr:lambda params rest-param body))
        (define env+
          (hash-set
           (for/fold ([env+ clo-env])
               ([p (in-list params)]
                [av (in-list arg-vals)])
             (hash-set env+ p av))
           rest-param
           (list-tail arg-vals (length params))))
        (define ctxt+
          (rack:evalctxt env+
                         (cons fresh-mark-frame
                               marks)))
        (define body-eval
          (interp ctxt+ body))

        (rack:eval:app:clo
         (rack:eval-vals body-eval)
         ctxt e
         fun-eval
         arg-evals
         body-eval)]
       [(rack:val:prim prim)
        (rack:eval:app:prim
         (prim arg-vals)
         ctxt e
         fun-eval
         arg-evals)])]
    [(rack:expr:ref sym)
     (rack:eval:ref
      (vector (hash-ref env sym))
      ctxt e)]))
