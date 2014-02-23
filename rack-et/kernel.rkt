#lang racket/base
(require racket/match
         racket/list)

;; make-block : ptrs bytes -> ptr

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

(struct rack:expr ())
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

(struct rack:val ())
(struct rack:val:clo rack:val
        (env lam))
(struct rack:val:prim rack:val
        (prim))

;; State

(struct rack:cstate
        (code env store kont))
(struct rack:vstate
        (vals kont store))

(define mt-ms (hash))

(struct rack:k (e k ms f))
(struct rack:k:top ())

(define (k-mark-set k mk mv)
  (match-define (rack:k e nk ms f) k)
  (define ms+
    (hash-set ms mk mv))
  (rack:k e nk ms+ f))

(struct rack:kf ())
(struct rack:kf:if0 rack:kf
        (zero nonzero))
(struct rack:kf:let-values rack:kf
        (ids body))
(struct rack:kf:wcm:key rack:kf
        (val body))
(struct rack:kf:wcm:val rack:kf
        (key body))
(struct rack:kf:app:fun rack:kf
        (args))
(struct rack:kf:app:args rack:kf
        (fun rvals args))

(define (rack:state:app fun rvals args e k s)
  (match args
    [(list)
     (define vals (list->vector (reverse rvals)))
     (match fun
       [(rack:val:clo e (rack:expr:lambda params rest-param body))
        (define e+
          (hash-set
           (for/fold ([e+ e])
               ([p (in-vector params)]
                [v (in-vector vals)])
             (hash-set e+ p v))
           rest-param
           (list-tail vals (length params))))
        (rack:cstate body e+ s k)]
       [(rack:val:prim prim)
        (rack:vstate (prim vals) k s)])]
    [(cons arg args)
     (rack:cstate arg e s
                  (rack:k e k mt-ms
                          (rack:kf:app:args fun rvals args)))]))

(define (step st)
  (match st
    [(rack:cstate c e s k)
     (match c
       [(rack:expr:lambda params rest-param body)
        (rack:vstate (vector (rack:val:clo e c)) k s)]
       [(rack:expr:if0 test zero nonzero)
        (rack:cstate test e s
                     (rack:k e k mt-ms
                             (rack:kf:if0 zero nonzero)))]
       [(rack:expr:let-values ids vals body)
        (rack:cstate vals e s
                     (rack:k e k mt-ms
                             (rack:kf:let-values ids body)))]
       [(rack:expr:quote d)
        (rack:vstate (vector d) k s)]
       [(rack:expr:wcm mk mv body)
        (rack:cstate mk e s
                     (rack:k e k mt-ms
                             (rack:kf:wcm:key mv body)))]
       [(rack:expr:app fun args)
        (rack:cstate fun e s
                     (rack:k e k mt-ms
                             (rack:kf:app:fun args)))]
       [(rack:expr:ref v)
        (rack:vstate (vector (hash-ref e v)) k s)])]
    [(rack:vstate vs (rack:k:top) s)
     st]
    [(rack:vstate vs (rack:k e k ms kf) s)
     (define (v) (vector-ref vs 0))
     (match kf
       [(rack:kf:if0 zero nonzero)
        (define next
          ;; xxx fix
          (if (zero? (v))
            zero
            nonzero))
        (rack:cstate next e s k)]
       [(rack:kf:let-values ids body)
        (define e+
          (for/fold ([e+ e])
              ([i (in-list ids)]
               [v (in-vector vs)])
            (hash-set e+ i v)))
        (rack:cstate body e+ s k)]
       [(rack:kf:wcm:key mv body)
        (rack:cstate mv e s (rack:k e k mt-ms (rack:kf:wcm:val (v) body)))]
       [(rack:kf:wcm:val mk body)
        (rack:cstate body e s (k-mark-set k mk (v)))]
       [(rack:kf:app:fun args)
        (rack:state:app (v) empty args e k s)]
       [(rack:kf:app:args fun rvals (list))
        (rack:state:app fun (cons (v) rvals) empty e k s)]
       [(rack:kf:app:args fun rvals (cons arg args))
        (rack:cstate
         arg e s
         (rack:k e k mt-ms
                 (rack:kf:app:args fun (cons (v) rvals) args)))])]))
