#lang racket/base
(require ffi/vector)
(module+ test
  (require rackunit))

(define (make-object pointer-count atomic-size)
  (define v (make-vector (add1 pointer-count) #f))
  (vector-set! v pointer-count (make-u32vector atomic-size))
  v)
(define (object-ptr-ref o i)
  (vector-ref o i))
(define (object-atomic-ref o i)
  (u32uvector-ref (vector-ref o (sub1 (vector-length o))) i))

(require (for-syntax racket/base
                     racket/list
                     syntax/parse))
(begin-for-syntax
  (struct static-layout (pointers atomics) #:prefab))
(define-syntax mt#rep
  (static-layout empty empty))
(define-syntax (define-layout stx)
  (syntax-parse stx
    [(_ struct:id parent:id (pfield:id ...) (afield:id ...))
     (let ()
       (match-define 
        (static-layout parent-ptrs parent-atoms)
        (syntax-local-value #'parent))
       (define struct-layout 
         (static-layout (append parent-ptrs
                                (list #'pfield ...))
                        (append parent-atoms
                                (list #'afield ...))))
       (syntax/loc stx
         (begin
           (define-syntax struct#rep
             #,struct-layout)
           (define (struct.pfield= o v)
             (object-ptr-set! o pfieldi v))
           ...
           (define (struct.pfield o)
             (object-ptr-ref o pfieldi))
           ...
           (define (new.struct field ...)
             (define o
               (make-object
                ,#(length (static-layout-points struct-layout))
                atomic-size))
             (o . struct.field= . field)
             ...
             o))))]))

(module+ test
  (define-layout IntCons mt#rep
    [first _int32]
    [rest _racket])
  (define c0 (IntCons 4 #f))
  (check-equal? (IntCons.first c0) 4)
  (check-equal? (IntCons.rest c0) #f)
  (define c1 (IntCons 5 c0))
  (check-equal? (IntCons.first c1) 5)
  (check-equal? (IntCons.rest c1) c0))
