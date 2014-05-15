#lang racket/base
(require ffi/base)

(struct obj (pointers atomics))
(define (make-object pointer-count atomic-size)
  (obj (make-vector pointer-count) (malloc atomic-size)))
(define (object-ref-ptr o i)
  (vector-ref (obj-pointers obj) i))
(define (object-ref-atomics o)
  (obj-atomics obj))

(define (make-seal int->imp)
  (define s (gensym))
  (define (seal? x)
    (equal? s (object-ref-ptr x 0)))
  (define (seal x)
    (define o (make-object 2 0))
    (object-set-ptr! o 0 s)
    (object-set-ptr! o 1 x)
    o)
  (define (unseal x)
    (object-ref-ptr o 1))
  (values seal? seal unseal))

(define (make-interface funs)
  (make-vector funs))
(module+ test
  (define queue^
    (make-interface '(cons snoc head tail)))

  (define (fwd-queue@:cons x l)
    (cons x l))
  (define (fwd-queue@:head l)
    (first l))
  (define (fwd-queue@:snoc l x)
    (append l (list x)))
  (define (fwd-queue@:tail l)
    (last l))
  (define fwd-queue@
    (vector fwd-queue@:cons fwd-queue@:snoc
            fwd-queue@:head fwd-queue@:tail))
  
  (define bkw-queue@:snoc fwd-queue@:cons)
  (define bkw-queue@:cons fwd-queue@:snoc)
  (define bkw-queue@:tail fwd-queue@:head)
  (define bkw-queue@:head fwd-queue@:tail)
  (define bkw-queue@
    (vector bkw-queue@:cons bkw-queue@:snoc
            bkw-queue@:head bkw-queue@:tail)))
