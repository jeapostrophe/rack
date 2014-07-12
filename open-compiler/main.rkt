#lang racket/base
(require racket/match
         racket/list)

(struct v () #:transparent)
(struct v:num v (n) #:transparent)
(struct v:bool v (b) #:transparent)
(struct v:str v (s) #:transparent)
(struct v:sym v (s) #:transparent)
(struct v:mt v () #:transparent)

(struct bv () #:transparent)
(struct bv:val bv (v) #:transparent)
(struct bv:clo bv (env arg body) #:transparent)
(struct bv:cons bv (f r) #:transparent)

(struct bc () #:transparent)
(struct bc:val bc (v) #:transparent)
(struct bc:app bc (rator rand) #:transparent)
(struct bc:lam bc (arg body) #:transparent)
(struct bc:ref bc (var) #:transparent)
(struct bc:prim-app bc (prim args) #:transparent)
(struct bc:prim bc (p) #:transparent)

(struct ic (props) #:transparent)
(struct ic:val ic (v) #:transparent)
(struct ic:cons ic (f r) #:transparent)

(define mt-props (hasheq))

(define (.eval bc)
  (.eval/rec (hasheq) bc))

(define (.eval/rec env bc)
  (match bc
    [(bc:val v)
     (bv:val v)]
    [(bc:prim-app prim args)
     (match* (prim (map (λ (a) (.eval/rec env a)) args))
       [((bc:prim '#%+) (list (bv:val (v:num r1)) (bv:val (v:num r2))))
        (bv:val (v:num (+ r1 r2)))])]
    [(bc:app rator rand)
     (match (.eval/rec env rator)
       [bv
        (error '.eval "Expected bv:clo: ~e" bv)])]
    [_
     (error '.eval "Evaluate byte-code: ~e" bc)]))

(define (.compile ic)
  (match ic
    [(ic:cons _ f r)
     (define fc (.compile f))
     (match fc
       [(bc:ref '#%+)
        (match r
          [(ic:cons _ r1 (ic:cons _ r2 (ic:val _ (v:mt))))
           (bc:prim-app (bc:prim '#%+)
                        (list (.compile r1)
                              (.compile r2)))]
          [_
           (error '.compile "#%+ bad syntax: ~e" ic)])]
       [_
        (bc:app fc (.compile r))])]
    [(ic:val _ (v:sym v))
     (bc:ref v)]
    [(ic:val _ v)
     (bc:val v)]
    [_
     (error '.compile "Compile intermediate-code: ~e" ic)]))

(define-syntax hash-set*
  (syntax-rules ()
    [(_ ht)
     ht]
    [(_ ht k v . more)
     (hash-set* (hash-set ht k v) . more)]))

(define (.parse fc)
  (define fc-props
    (hash-set* mt-props
               'srcloc (v:num (random 50))
               'mark 0))
  (match fc
    [(cons f r)
     (ic:cons fc-props (.parse f) (.parse r))]
    [(? symbol? s)
     (ic:val fc-props (v:sym s))]
    [(? number? n)
     (ic:val fc-props (v:num n))]
    [(? empty?)
     (ic:val fc-props (v:mt))]
    [_
     (error '.parse "Parse front-end-code: ~e" fc)]))

(define (.main fc)
  (.eval (.compile (.parse fc))))

(module+ test
  (require rackunit)
  (check-equal?
   (.main '(#%+ 1 1))
   (bv:val (v:num 2))))
