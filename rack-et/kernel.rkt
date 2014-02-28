#lang racket/base
(require racket/match
         racket/list
         data/gvector)

;; C without operator overloading, but with GC, closures, and
;; continuations, FFI to C

;; XXX module system?
;; XXX custodians?

(define-syntax-rule (define-prim* i ...)
  (define prims
    (make-hasheq
     (list (cons 'i #f) ...))))

(define-prim*
  ;; conts
  call-with-escape-continuation
  call-with-current-continuation
  call-with-continuation-barrier
  call-with-continuation-prompt
  abort-current-continuation
  continuation-prompt-available?
  continuation-marks
  dynamic-wind
  ;; block
  make-block         ;; ptr-count byte-count -> block
  block-ptr-ref      ;; block i -> ptr
  block-ptr-set!
  block-byte-ref/i32 ;; block i -> i32
  block-byte-set!
  ;; seals
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
  malloc ;; does not contain ptrs and is not movable/managed by gc
  free
  make-immobile-box ;; contains one ptr but itself is not movable/managed by gc
  ;; xxx other kinds of malloc
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

;; Primitives

;; xxx more arithmetic (see llvm), plus deal with different kinds
(define (prim:v:add vs)
  (match-define (vector (rack:val:num l) (rack:val:num r)) vs)
  (vector (rack:val:num (+ l r))))

(define (prim:v:values vs)
  vs)

;; This was a bit of an ephiphany for me. call-with-values requires a
;; new type of continuation frame because it does something unique
;; after the generator has been called. This makes it seem like it
;; should be core syntax. On the other hand, it requires normal
;; argument evaluation of the pieces before hand (i.e. it doesn't run
;; the generator before it figures out what the receiver is), so it
;; would have to duplicate some of the arg handling code. Similarly,
;; it would have to duplicate some of the function calling code, if
;; not for this approach. Finally, this is an example of how
;; primitives will be statically broken into different categories
;; based on what they do: pure, reader, writer, unpredictable. It
;; would be nice, however, to clarify that this is pure provided
;; generator is pure, but I suppose that's not really meaningful
(define (prim:st:call-with-values vs s k)
  (match-define (vector generator receiver) vs)
  (rack:apply generator (vector) s
              (rack:k mt-e k mt-ms
                      (rack:kf:call-with-values:values
                       receiver))))

;; Code

(define-syntax-rule (structt s ...)
  (struct s ... #:transparent))

(structt rack:expr ())
(structt rack:expr:lambda rack:expr
         (params rest-param body))
(structt rack:expr:if0 rack:expr
         (test zero nonzero))
(structt rack:expr:quote rack:expr
         (datum))
(structt rack:expr:wcm rack:expr
         (key val body))
(structt rack:expr:app rack:expr
         (fun args))
(structt rack:expr:ref rack:expr
         (sym))

;; Values

(structt rack:val ())
(structt rack:val:num rack:val
         (num))
(structt rack:val:clo rack:val
         (env lam))
(structt rack:val:prim:v rack:val
         (prim))
(structt rack:val:prim:st rack:val
         (prim))

;; State

(structt rack:cstate
         (code env store kont))
(structt rack:vstate
         (vals store kont))

(define mt-e (hasheq))
(define mt-ms (hash))

(structt rack:k (e k ms f))
(structt rack:k:top ())

(define (k-mark-set k mk mv)
  (match-define (rack:k e nk ms f) k)
  (define ms+
    (hash-set ms mk mv))
  (rack:k e nk ms+ f))

(structt rack:kf ())
(structt rack:kf:if0 rack:kf
         (zero nonzero))
(structt rack:kf:wcm:key rack:kf
         (val body))
(structt rack:kf:wcm:val rack:kf
         (key body))
(structt rack:kf:app:fun rack:kf
         (args))
(structt rack:kf:app:args rack:kf
         (fun rvals args))
(structt rack:kf:call-with-values:values rack:kf
         (receiver))

;; xxx remove cons on args and replace with vector + i

(define (step st)
  (match st
    [(rack:cstate c e s k)
     (match c
       [(rack:expr:lambda params rest-param body)
        ;;; xxx shrink e
        (rack:vstate (vector (rack:val:clo e c)) s k)]
       [(rack:expr:if0 test zero nonzero)
        (rack:cstate test e s
                     ;;; xxx shrink e given zero/nonzero
                     (rack:k e k mt-ms
                             (rack:kf:if0 zero nonzero)))]
       [(rack:expr:quote d)
        (rack:vstate (vector d) s k)]
       [(rack:expr:wcm mk mv body)
        (rack:cstate mk e s
                     ;;; xxx shrink e
                     (rack:k e k mt-ms
                             (rack:kf:wcm:key mv body)))]
       [(rack:expr:app fun args)
        (rack:cstate fun e s
                     ;;; xxx shrink e
                     (rack:k e k mt-ms
                             (rack:kf:app:fun args)))]
       [(rack:expr:ref v)
        (rack:vstate (vector (hash-ref e v)) s k)]
       [_
        (error 'step "Unknown c: ~e" c)])]
    [(rack:vstate vs s (rack:k:top))
     st]
    [(rack:vstate vs s (rack:k e k ms kf))
     (define (v) (vector-ref vs 0))
     (match kf
       [(rack:kf:if0 zero nonzero)
        (define next
          (match (v)
            [(rack:val:num 0)
             zero]
            [_
             nonzero]))
        (rack:cstate next e s k)]
       [(rack:kf:wcm:key mv body)
        ;;; xxx shrink second e?
        (rack:cstate mv e s (rack:k e k mt-ms (rack:kf:wcm:val (v) body)))]
       [(rack:kf:wcm:val mk body)
        (rack:cstate body e s (k-mark-set k mk (v)))]
       [(rack:kf:app:fun args)
        (rack:state:app (v) empty args e s k)]
       [(rack:kf:app:args fun rvals (list))
        (rack:state:app fun (cons (v) rvals) empty e s k)]
       [(rack:kf:app:args fun rvals (cons arg args))
        (rack:cstate
         arg e s
         ;;; xxx shrink e
         (rack:k e k mt-ms
                 (rack:kf:app:args fun (cons (v) rvals) args)))]
       [(rack:kf:call-with-values:values receiver)
        (rack:apply receiver vs s k)]
       [_
        (error 'step "Unknown kf: ~e" kf)])]
    [_
     (error 'step "Unknown st: ~e" st)]))

(define (rack:state:app fun rvals args e s k)
  (match args
    [(list)
     (define vals (list->vector (reverse rvals)))
     ;;; xxx provide e?
     (rack:apply fun vals s k)]
    [(cons arg args)
     (rack:cstate arg e s
                  ;;; xxx shrink e
                  (rack:k e k mt-ms
                          (rack:kf:app:args fun rvals args)))]))

(define (rack:apply fun vals s k)
  (match fun
    [(rack:val:clo e (rack:expr:lambda params rest-param body))
     (define e+params
       (for/fold ([e+ e])
           ([p (in-vector params)]
            [v (in-vector vals)])
         (hash-set e+ p v)))
     (define e+
       (if rest-param
         (let ()
           (define val-cnt (vector-length vals))
           (define param-cnt (vector-length params))
           (define cnt (- val-cnt param-cnt))
           (define rest-vals
             (make-vector (add1 cnt)))
           (vector-set! rest-vals 0 cnt)
           (vector-copy! rest-vals 1 vals param-cnt)
           (hash-set
            e+params
            rest-param
            rest-vals))
         e+params))
     (rack:cstate body e+ s k)]
    [(rack:val:prim:v prim)
     (rack:vstate (prim vals) s k)]
    [(rack:val:prim:st prim)
     ;;; xxx get and provide e?
     (prim vals s k)]))

(define (step* st)
  (define st-p (step st))
  (if (eq? st-p st)
    st
    (step* st-p)))

(define (inject c)
  (rack:cstate c mt-e (make-gvector) (rack:k:top)))

(module+ test
  (require rackunit)
  (define-syntax-rule (test c vs)
    (check-equal? (rack:vstate-vals (step* (inject c))) vs))
  (define-syntax-rule (tests [c vs] ...)
    (begin (test c vs) ...))

  (tests
   ;; quote
   [(rack:expr:quote (rack:val:num 0))
    (vector (rack:val:num 0))]
   ;; if0
   [(rack:expr:if0 (rack:expr:quote (rack:val:num 0))
                   (rack:expr:quote (rack:val:num 1))
                   (rack:expr:quote (rack:val:num 2)))
    (vector (rack:val:num 1))]
   [(rack:expr:if0 (rack:expr:quote (rack:val:num 3))
                   (rack:expr:quote (rack:val:num 1))
                   (rack:expr:quote (rack:val:num 2)))
    (vector (rack:val:num 2))]
   ;; app prim
   [(rack:expr:app (rack:expr:quote (rack:val:prim:v prim:v:add))
                   (list (rack:expr:quote (rack:val:num 1))
                         (rack:expr:quote (rack:val:num 2))))
    (vector (rack:val:num 3))]
   [(rack:expr:app (rack:expr:quote (rack:val:prim:v prim:v:values))
                   (list (rack:expr:quote (rack:val:num 1))
                         (rack:expr:quote (rack:val:num 2))))
    (vector (rack:val:num 1)
            (rack:val:num 2))]
   [(rack:expr:app (rack:expr:quote (rack:val:prim:v prim:v:values))
                   (list))
    (vector)]
   ;; lambda + ref
   ;;; no args
   [(rack:expr:app
     (rack:expr:lambda (vector) #f
                       (rack:expr:quote (rack:val:num 0)))
     (list))
    (vector (rack:val:num 0))]
   ;;; 1 arg
   [(rack:expr:app
     (rack:expr:lambda (vector 'x) #f
                       (rack:expr:ref 'x))
     (list (rack:expr:quote (rack:val:num 0))))
    (vector (rack:val:num 0))]
   ;;; n args
   [(rack:expr:app
     (rack:expr:lambda (vector 'x 'y) #f
                       (rack:expr:app (rack:expr:quote (rack:val:prim:v prim:v:values))
                                      (list (rack:expr:ref 'x)
                                            (rack:expr:ref 'y))))
     (list (rack:expr:quote (rack:val:num 0))
           (rack:expr:quote (rack:val:num 1))))
    (vector (rack:val:num 0)
            (rack:val:num 1))]
   ;;; rest args
   [(rack:expr:app
     (rack:expr:lambda (vector 'x) 'y
                       (rack:expr:app (rack:expr:quote (rack:val:prim:v prim:v:values))
                                      (list (rack:expr:ref 'x)
                                            (rack:expr:ref 'y))))
     (list (rack:expr:quote (rack:val:num 0))
           (rack:expr:quote (rack:val:num 1))))
    (vector (rack:val:num 0)
            (vector 1 (rack:val:num 1)))]
   [(rack:expr:app
     (rack:expr:quote (rack:val:prim:st prim:st:call-with-values))
     (list
      (rack:expr:lambda
       (vector) #f
       (rack:expr:app (rack:expr:quote (rack:val:prim:v prim:v:values))
                      (list (rack:expr:quote (rack:val:num 0))
                            (rack:expr:quote (rack:val:num 1)))))
      (rack:expr:quote (rack:val:prim:v prim:v:add))))
    (vector (rack:val:num 1))]
   [(rack:expr:app
     (rack:expr:quote (rack:val:prim:st prim:st:call-with-values))
     (list
      (rack:expr:lambda
       (vector) #f
       (rack:expr:app (rack:expr:quote (rack:val:prim:v prim:v:values))
                      (list (rack:expr:quote (rack:val:num 0))
                            (rack:expr:quote (rack:val:num 1)))))
      (rack:expr:lambda
       (vector) 'x
       (rack:expr:ref 'x))))
    (vector (vector 2 (rack:val:num 0) (rack:val:num 1)))]
   ;; let-values + ref
   [(let ()
      (define (rack:expr:let-values ids vals body)
        (rack:expr:app
         (rack:expr:quote (rack:val:prim:st prim:st:call-with-values))
         (list
          (rack:expr:lambda (vector) #f vals)
          (rack:expr:lambda ids #f body))))

      (rack:expr:let-values (vector 'x)
                            (rack:expr:quote (rack:val:num 0))
                            (rack:expr:ref 'x)))
    (vector (rack:val:num 0))]
   ;; wcm
   ;; app
   ;; app prims
   ;; app clos
   ))

;;; xxx add module + top-level references

;; module : requires x (id -> refs x expr) [refs is set of ids to
;; call/eval before evaling expr to a value (for effects)]

;; notice that we don't have phase information there, because macros
;; are part of the compiler and are gone when we get to the run-time
;; system.

;; provides: are they just a list of ids? do we expose everything but
;; use name obfuscation? do we integrate the interface idea?

;; a file is not a module, but is a collection of modules

;; module-repo : module-name -> module
;; execute : module-repo module-name id -> value
