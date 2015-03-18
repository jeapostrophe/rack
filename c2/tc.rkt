#lang racket/base
(require racket/list
         racket/match
         racket/bool)

(define name=? symbol=?)

(struct term () #:prefab)
(struct t:Set term () #:prefab)
(define Set (t:Set))
(struct t:Prop term () #:prefab)
(define Prop (t:Prop))
(struct t:Type term (i) #:prefab)

(define (t:Type-max T U)
  (t:Type (max (t:Type-i T) (t:Type-i U))))
(define (t:Type-add1 T)
  (t:Type (add1 (t:Type-i T))))

(define (in-S? t)
  (match t
    [(t:Set) #t]
    [(t:Prop) #t]
    [(t:Type i) #t]
    [_ #f]))

(struct t:global term (name) #:prefab)
(struct t:local term (name) #:prefab)
;; \forall x : T, U
(struct t:forall term (x T U) #:prefab)
;; fun x : T => U
(struct t:fun term (x T U) #:prefab)
;; T U
(struct t:app term (T U) #:prefab)
;; let x := T in U
(struct t:let term (x T U) #:prefab)

(define (term-subst-binding from-name to-term in-term t:cons x T U)
  (cond
   [(name=? from-name x)
    in-term]
   [else
    (define new-x (gensym x))
    (t:cons new-x
            (term-subst from-name to-term T)
            (term-subst from-name to-term
                        (term-subst x (t:local new-x) U)))]))
(define (term-subst from-name to-term in-term)
  (match in-term
    [(t:global _) in-term]
    [(t:local name)
     (if (name=? from-name name)
         to-term
         in-term)]
    [(t:forall x T U)
     (term-subst-binding from-name to-term in-term t:forall x T U)]
    [(t:fun x T U)
     (term-subst-binding from-name to-term in-term t:fun x T U)]
    [(t:app T U)
     (t:app (term-subst from-name to-term T)
            (term-subst from-name to-term U))]
    [(t:let x T U)
     (term-subst-binding from-name to-term in-term t:let x T U)]))

(struct decl () #:prefab)
;; x : T
(struct assm decl (x T) #:prefab)
;; x := t : T
(struct assm-defn assm (t) #:prefab)
(define (defn x t T)
  (assm-defn x T t))

(define ctxt-empty empty)
(define ctxt-cons cons)
(define (ctxt-lookup a-ctxt x)
  (findf (λ (d) (name=? (assm-x d) x)) a-ctxt))

(define env-empty ctxt-empty)
(define env-cons ctxt-cons)
(define env-lookup ctxt-lookup)

(define (type-of E Γ t)
  (match t
    [(t:Prop)
     (t:Type 0)]
    [(t:Set)
     (t:Type 0)]
    [(t:Type i)
     (t:Type (add1 i))]
    [(t:global c)
     (match (env-lookup E c)
       [#f #f]
       [(assm _ T) T])]
    [(t:local x)
     (match (ctxt-lookup Γ x)
       [#f #f]
       [(assm _ T) T])]
    [(t:forall x T U)
     (define s (type-of E Γ T))
     (cond
      [(in-S? s)
       (define Ut (type-of E (ctxt-cons (assm x T) Γ) U))
       (cond
        [(and (t:Prop? Ut))
         Ut]
        [(and (or (t:Prop? s) (t:Set? s))
              (t:Set? Ut))
         Ut]
        [(and (t:Type? s)
              (t:Type? Ut))
         (t:Type-add1 (t:Type-max s Ut))]
        [else
         (list "t:forall rule inside in-S?"
               E Γ t x T U s Ut)])]
      [else
       (list "t:forall rule outside in-S?"
             E Γ t x T U s)])]
    [(t:fun x T t)
     (define U (type-of E (ctxt-cons (assm x T) Γ) t))
     (define maybe-ty (t:forall x T U))
     (define s (type-of E Γ maybe-ty))
     (and (term? U) (term? s) maybe-ty)]
    [(t:app t u)
     (match-define (t:forall x U_lhs T) (type-of E Γ t))
     (define U_rhs (type-of E Γ u))
     (if (type=? E Γ U_lhs U_rhs)
         (term-subst x u T)
         #f)]
    [(t:let x t u)
     (define T (type-of E Γ t))
     (define U (type-of E (ctxt-cons (defn x t T) Γ) u))
     (term-subst x t U)]))

(define (term=? E Γ T U)
  ;; xxx modulo evaluation
  ;; xxx conversion to de-Bruijin
  (equal? T U))

(define type=? term=?)

;; XXX We're at 4.3 of https://coq.inria.fr/refman/Reference-Manual006.html

(define (t:impl A B)
  (t:forall (gensym 'impl) A B))
(define (t:and A B)
  (t:forall 'C Prop
            (t:impl
             (t:impl A (t:impl B (t:local 'C)))
             (t:local 'C))))
(define (t:or A B)
  (t:forall 'C Prop
            (t:impl (t:impl A (t:local 'C))
                    (t:impl (t:impl B (t:local 'C))
                            (t:local 'C)))))
(define (t:neg A)
  (t:forall 'C Prop
            (t:impl A (t:local 'C))))
(define (t:exists x A B)
  (t:forall 'C Prop
            (t:impl (t:forall x A (t:impl B (t:local 'C)))
                    (t:local 'C))))

(module+ test
  (require racket/pretty)

  ;; ((A \/ B) -> C) -> ((B -> C) \/ (A -> C))

  (pretty-print
   (type-of env-empty
            (ctxt-cons (assm 'A Prop)
                       (ctxt-cons (assm 'B Prop)
                                  (ctxt-cons (assm 'C Prop)
                                             ctxt-empty)))
            (t:impl (t:impl (t:or (t:local 'A) (t:local 'B))
                            (t:local 'C))
                    (t:or (t:impl (t:local 'A) (t:local 'C))
                          (t:impl (t:local 'B) (t:local 'C)))))))
