#lang racket/base
(require racket/match
         unstable/match
         rack/ck/ast
         rack/ck/reader)
(module+ test
  (require rackunit))

(module+ test
  (define (pp v)
    (match v
      [(term:ast-file _ l c)
       (cons (pp l) (pp c))]
      [(term:ast:str _ s)
       s]
      [(term:ast:id _ s)
       s]
      [(term:ast:num _ s)
       s]
      [(term:ast:list:empty _)
       null]
      [(term:ast:list:cons _ f r)
       (cons (pp f) (pp r))]
      ;; surface
      [(term:surface:str _ s)
       s]
      [(term:surface:num _ s)
       s]
      [(term:surface:group _ s)
       (pp s)]
      [(term:surface:swap _ s)
       (pp s)]
      [(term:surface:list:empty _)
       null]
      [(term:surface:list:cons _ f r)
       (cons (pp f) (pp r))]
      [(term:surface:id _ s)
       s]
      [(term:surface:op _ s)
       (pp s)]
      [(term loc)
       (en-error loc (format "pp: unexpected term: ~e" v))])))

(define (term:ast:list-append x y)
  (match x
    [(term:ast:list:empty _)
     y]
    [(term:ast:list:cons loc f r)
     ;; xxx should i loc-merge?
     (term:ast:list:cons loc f (term:ast:list-append r y))]
    [(term loc)
     (en-error loc (format "list-append: unexpected term: ~e" x))]))

;; xxx term:ast:list-append

(define (loc-merge x y)
  (match-define (srcloc src1 line1 col1 pos1 span1) x)
  (match-define (srcloc src2 line2 col2 pos2 span2) y)
  (srcloc src1 line1 col1 pos1 (- (+ pos2 span2) pos1)))

;; xxx loc-merge

(define (str-merge s)
  (match s
    [(term:ast:list:empty loc)
     s]
    [(term:ast:list:cons
      loc1 (term:ast:str loc2 s1)
      (term:ast:list:cons
       loc3 (term:ast:str loc4 s2)
       more))
     (str-merge
      (term:ast:list:cons
       (loc-merge loc1 loc3)
       (term:ast:str (loc-merge loc2 loc4) (string-append s1 s2))
       more))]
    [(term:ast:list:cons
      loc1 before after)
     (term:ast:list:cons
      loc1 before (str-merge after))]
    [(term loc)
     (en-error loc (format "str-merge: unexpected term: ~e" s))]))

;; xxx str-merge

(define (en-error loc msg)
  (error 'en "~a: ~a" loc msg))

(define (op? v)
  (or (term:surface:op? v)
      (and (term:surface:swap? v)
           (not (op? (term:surface:swap-t v))))))
(define (op-id v)
  (match v
    [(term:surface:op _ t)
     (op-id t)]
    [(term:surface:swap _ t)
     (op-id t)]
    [(term:surface:id _ s)
     s]
    [else
     #f]))

(module+ test
  (define l0 (srcloc #f #f #f #f #f))
  (define (term:surface:list . l)
    (foldr (λ (e a)
             (term:surface:list:cons l0 e a))
           (term:surface:list:empty l0)
           l))
  (define (term:ast:list . l)
    (foldr (λ (e a)
             (term:ast:list:cons l0 e a))
           (term:ast:list:empty l0)
           l))

  (define-syntax-rule (check-en* en i eo)
    (test-case
     (format "(~a ~v)" 'en (pp i))
     (let ()
       (define ao (en i))
       (if (equal? ao eo)
         (check-equal? ao eo)
         (fail (format "expected ~a, got ~a"
                       (pp eo) (pp ao))))))))

(define (surface-group s)
  (define (surface-until-op l)
    (match l
      [(? term:surface:list:empty?)
       (values l l)]
      [(term:surface:list:cons loc f r)
       (cond
         [(op? f)
          (values (term:surface:list:empty loc) l)]
         [else
          (define-values (until-op more) (surface-until-op r))
          (values (term:surface:list:cons loc f until-op)
                  more)])]
      [(term loc)
       (en-error loc (format "surface-until-op: unexpected term: ~e" s))]))
  (define (singleton-extract l)
    (match l
      [(term:surface:list:cons _ f (term:surface:list:empty _))
       f]
      [_
       l]))
  (define (move-forward-two l)
    (match l
      [(? term:surface:list:empty?)
       l]
      [(term:surface:list:cons loc f r)
       (term:surface:list:cons loc f (surface-group r))]
      [(term loc)
       (en-error loc (format "move-forward-two: unexpected term: ~e" s))]))

  (match s
    [(? term:surface:list:empty?)
     s]
    [(term:surface:list:cons loc _ _)
     (define-values (until-op more)
       (surface-until-op s))
     (if (term:surface:list:cons? until-op)
       (term:surface:list:cons
        loc
        (singleton-extract until-op)
        (move-forward-two more))
       (move-forward-two more))]
    [(term loc)
     (en-error loc (format "surface-group: unexpected term: ~e" s))]))

(module+ test
  (define-syntax-rule (check-group i eo)
    (check-en* surface-group i eo))

  (check-group
   (term:surface:list)
   (term:surface:list))

  (check-group
   (term:surface:list
    (term:surface:id l0 'a))
   (term:surface:list
    (term:surface:id l0 'a)))

  (check-group
   (term:surface:list
    (term:surface:id l0 'a)
    (term:surface:id l0 'b))
   (term:surface:list
    (term:surface:list
     (term:surface:id l0 'a)
     (term:surface:id l0 'b))))

  (check-group
   (term:surface:list
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:id l0 'a))
   (term:surface:list
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:id l0 'a)))

  (check-group
   (term:surface:list
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:id l0 'a)
    (term:surface:id l0 'b))
   (term:surface:list
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:list (term:surface:id l0 'a)
                       (term:surface:id l0 'b))))

  (check-group
   (term:surface:list
    (term:surface:id l0 'a)
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:id l0 'c))
   (term:surface:list
    (term:surface:id l0 'a)
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:id l0 'c)))

  (check-group
   (term:surface:list
    (term:surface:id l0 'a)
    (term:surface:id l0 'b)
    (term:surface:swap l0 (term:surface:id l0 'OP))
    (term:surface:id l0 'c)
    (term:surface:id l0 'd))
   (term:surface:list
    (term:surface:list (term:surface:id l0 'a)
                       (term:surface:id l0 'b))
    (term:surface:swap l0 (term:surface:id l0 'OP))
    (term:surface:list (term:surface:id l0 'c)
                       (term:surface:id l0 'd))))

  (check-group
   (term:surface:list
    (term:surface:id l0 'a)
    (term:surface:id l0 'b)
    (term:surface:swap
     l0 (term:surface:swap l0 (term:surface:op l0 (term:surface:id l0 '+))))
    (term:surface:id l0 'c)
    (term:surface:id l0 'd))
   (term:surface:list
    (term:surface:list (term:surface:id l0 'a)
                       (term:surface:id l0 'b))
    (term:surface:swap
     l0 (term:surface:swap l0 (term:surface:op l0 (term:surface:id l0 '+))))
    (term:surface:list (term:surface:id l0 'c)
                       (term:surface:id l0 'd))))

  (check-group
   (term:surface:list
    (term:surface:id l0 'a)
    (term:surface:id l0 'b)
    (term:surface:swap l0 (term:surface:op l0 (term:surface:id l0 '+)))
    (term:surface:id l0 'c)
    (term:surface:id l0 'd))
   (term:surface:list
    (term:surface:list
     (term:surface:id l0 'a)
     (term:surface:id l0 'b)
     (term:surface:swap l0 (term:surface:op l0 (term:surface:id l0 '+)))
     (term:surface:id l0 'c)
     (term:surface:id l0 'd))))

  (check-group
   (term:surface:list
    (term:surface:list (term:surface:id l0 'a)
                       (term:surface:id l0 'b))
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:id l0 'c))
   (term:surface:list
    (term:surface:list (term:surface:id l0 'a)
                       (term:surface:id l0 'b))
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:id l0 'c)))

  (check-group
   (term:surface:list
    (term:surface:id l0 'a)
    (term:surface:id l0 'b)
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:id l0 'c))
   (term:surface:list
    (term:surface:list (term:surface:id l0 'a)
                       (term:surface:id l0 'b))
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:id l0 'c)))

  (check-group
   (term:surface:list
    (term:surface:id l0 'a)
    (term:surface:id l0 'b)
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:id l0 'c)
    (term:surface:id l0 'd))
   (term:surface:list
    (term:surface:list (term:surface:id l0 'a)
                       (term:surface:id l0 'b))
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:list (term:surface:id l0 'c)
                       (term:surface:id l0 'd))))

  (check-group
   (term:surface:list
    (term:surface:id l0 'a)
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:id l0 'b)
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:id l0 'c)
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:id l0 'd))
   (term:surface:list
    (term:surface:id l0 'a)
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:id l0 'b)
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:id l0 'c)
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:id l0 'd))))

(struct prec-state (output operators))
(define (surface-precedence s)
  (define fake-loc (srcloc #f #f #f #f #f))
  (define (E input st)
    (match input
      [(term:surface:list:empty loc)
       (pop-rators st)]
      [(term:surface:list:cons loc f r)
       (if (op? f)
         (E r (push-rator loc f st))
         (E r (push-rand loc f st)))]
      [(term loc)
       (en-error loc (format "prec/E: unexpected term: ~e" s))]))
  (define (pop-rators st)
    (match st
      [(prec-state (term:surface:list:cons _ f (term:surface:list:empty _))
                   (term:surface:list:empty _))
       f]
      [_
       (pop-rators (pop-rator st))]))
  (define (push-rand loc n st)
    (match-define (prec-state rands rators) st)
    (prec-state (term:surface:list:cons loc n rands) rators))
  (define (push-rator loc op st)
    (cond
      [(op-precedence< op (top-op st))
       (push-rator loc op (pop-rator st))]
      [else
       (match-define (prec-state rands rators) st)
       (prec-state rands (term:surface:list:cons loc op rators))]))
  (define (top-op st)
    (match-define (prec-state rands rators) st)
    (match rators
      [(term:surface:list:empty _)
       #f]
      [(term:surface:list:cons _ op _)
       op]
      [(term loc)
       (en-error loc (format "prec/top-op: unexpected term: ~e" s))]))
  (define (op-precedence o)
    (case (op-id o)
      [(-> |.|)
       100]
      [(! ~ ++ -- &)
       90]
      [(* / %)
       80]
      [(+ -)
       70]
      [(<< >>)
       60]
      [(< <= > >=)
       50]
      [(&)
       40]
      [(^)
       30]
      [(&&)
       20]
      [(\|\|)
       10]
      [(|,|)
       5]
      [(=)
       -10]
      [(|;|)
       -20]
      [else
       0]))
  (define (op-precedence< y x)
    (match* (x y)
      [(#f _)
       #f]
      [(_ #f)
       (error 'op-precedence< "impossible case: ~e ~e\n" y x)]
      [(_ _)
       (< (op-precedence y)
          (op-precedence x))]))
  (define (pop-rator st)
    (match st
      [(prec-state
        (term:surface:list:cons
         rand-loc-1 rand1
         (term:surface:list:cons rand-loc-0 rand0 rands))
        (term:surface:list:cons rator-loc rator rators))
       (prec-state
        (term:surface:list:cons
         rator-loc
         (term:surface:list:cons
          rator-loc rator
          (term:surface:list:cons
           rand-loc-0 rand0
           (term:surface:list:cons
            rand-loc-1 rand1
            (term:surface:list:empty rand-loc-1))))
         rands)
        rators)]
      [(prec-state
        (term:surface:list:cons
         rand-loc-0 rand0
         (term:surface:list:empty mt-loc))
        (term:surface:list:cons rator-loc rator rators))
       (prec-state
        (term:surface:list:cons
         rator-loc
         (term:surface:list:cons
          rator-loc rator
          (term:surface:list:cons
           rand-loc-0 rand0
           (term:surface:list:empty mt-loc)))
         (term:surface:list:empty mt-loc))
        rators)]
      [(term loc)
       (en-error loc (format "prec/pop-op: unexpected term: ~e" s))]))
  (E s
     (prec-state
      (term:surface:list:empty fake-loc)
      (term:surface:list:empty fake-loc))))

(module+ test
  (define-syntax-rule (check-prec i eo)
    (check-en* surface-precedence i eo))

  (check-prec
   (term:surface:list
    (term:surface:id l0 'i)
    (term:surface:op l0 (term:surface:id l0 '<))
    (term:surface:id l0 'width)
    (term:surface:op l0 (term:surface:id l0 '-))
    (term:surface:id l0 'twenty))
   (term:surface:list
    (term:surface:op l0 (term:surface:id l0 '<))
    (term:surface:id l0 'i)
    (term:surface:list
     (term:surface:op l0 (term:surface:id l0 '-))
     (term:surface:id l0 'width)
     (term:surface:id l0 'twenty))))

  (check-prec
   (term:surface:list
    (term:surface:id l0 'i)
    (term:surface:op l0 (term:surface:id l0 '>))
    (term:surface:id l0 'thirty)
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:id l0 'seventy))
   (term:surface:list
    (term:surface:op l0 (term:surface:id l0 '>))
    (term:surface:id l0 'i)
    (term:surface:list
     (term:surface:op l0 (term:surface:id l0 '+))
     (term:surface:id l0 'thirty)
     (term:surface:id l0 'seventy))))

  (check-prec
   (term:surface:list
    (term:surface:id l0 'four)
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:id l0 'two)
    (term:surface:op l0 (term:surface:id l0 '*))
    (term:surface:id l0 'eight))
   (term:surface:list
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:id l0 'four)
    (term:surface:list
     (term:surface:op l0 (term:surface:id l0 '*))
     (term:surface:id l0 'two)
     (term:surface:id l0 'eight))))

  (check-prec
   (term:surface:list
    (term:surface:id l0 'i)
    (term:surface:op l0 (term:surface:id l0 '>))
    (term:surface:num l0 20)
    (term:surface:op l0 (term:surface:id l0 '&&))
    (term:surface:id l0 'i)
    (term:surface:op l0 (term:surface:id l0 '<))
    (term:surface:num l0 50)
    (term:surface:op l0 (term:surface:id l0 (string->symbol "||")))
    (term:surface:id l0 'i)
    (term:surface:op l0 (term:surface:id l0 '>))
    (term:surface:num l0 100)
    (term:surface:op l0 (term:surface:id l0 '&&))
    (term:surface:id l0 'i)
    (term:surface:op l0 (term:surface:id l0 '<))
    (term:surface:id l0 'width)
    (term:surface:op l0 (term:surface:id l0 '-))
    (term:surface:num l0 20))
   (term:surface:list
    (term:surface:op l0 (term:surface:id l0 (string->symbol "||")))
    (term:surface:list
     (term:surface:op l0 (term:surface:id l0 '&&))
     (term:surface:list
      (term:surface:op l0 (term:surface:id l0 '>))
      (term:surface:id l0 'i)
      (term:surface:num l0 20))
     (term:surface:list
      (term:surface:op l0 (term:surface:id l0 '<))
      (term:surface:id l0 'i)
      (term:surface:num l0 50)))
    (term:surface:list
     (term:surface:op l0 (term:surface:id l0 '&&))
     (term:surface:list
      (term:surface:op l0 (term:surface:id l0 '>))
      (term:surface:id l0 'i)
      (term:surface:num l0 100))
     (term:surface:list
      (term:surface:op l0 (term:surface:id l0 '<))
      (term:surface:id l0 'i)
      (term:surface:list
       (term:surface:op l0 (term:surface:id l0 '-))
       (term:surface:id l0 'width)
       (term:surface:num l0 20))))))

  (check-prec
   (term:surface:list
    (term:surface:num l0 4)
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:num l0 2)
    (term:surface:op l0 (term:surface:id l0 '*))
    (term:surface:num l0 8)
    (term:surface:op l0 (term:surface:id l0 '|,|))
    (term:surface:num l0 52)
    (term:surface:op l0 (term:surface:id l0 '-))
    (term:surface:num l0 2))
   (term:surface:list
    (term:surface:op l0 (term:surface:id l0 '|,|))
    (term:surface:list
     (term:surface:op l0 (term:surface:id l0 '+))
     (term:surface:num l0 4)
     (term:surface:list
      (term:surface:op l0 (term:surface:id l0 '*))
      (term:surface:num l0 2)
      (term:surface:num l0 8)))
    (term:surface:list
     (term:surface:op l0 (term:surface:id l0 '-))
     (term:surface:num l0 52)
     (term:surface:num l0 2))))

  (check-prec
   (term:surface:list
    (term:surface:op l0 (term:surface:id l0 '-))
    (term:surface:num l0 4)
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:num l0 2)
    (term:surface:op l0 (term:surface:id l0 '*))
    (term:surface:num l0 8))
   (term:surface:list
    (term:surface:op l0 (term:surface:id l0 '+))
    (term:surface:list
     (term:surface:op l0 (term:surface:id l0 '-))
     (term:surface:num l0 4))
    (term:surface:list
     (term:surface:op l0 (term:surface:id l0 '*))
     (term:surface:num l0 2)
     (term:surface:num l0 8)))))

;; (en)forest : surface -> ast
(define (en s)
  (match s
    [(term:surface:str loc s)
     (term:ast:str loc s)]
    [(term:surface:id loc s)
     (term:ast:id loc s)]
    [(term:surface:op loc s)
     (en s)]
    [(term:surface:parens loc s)
     (en s)]
    [(term:surface:swap loc s)
     (en s)]
    [(term:surface:group loc s)
     (en (surface-precedence (surface-group s)))]
    [(term:surface:braces loc s)
     (term:ast:list:cons
      loc
      (term:ast:id loc '#%braces)
      (en s))]
    [(term:surface:brackets loc s)
     (term:ast:list:cons
      loc
      (term:ast:id loc '#%brackets)
      (en s))]
    [(term:surface:num loc s)
     (term:ast:num loc s)]
    [(term:surface:text loc l)
     (str-merge (en l))]
    [(term:surface:list:empty loc)
     (term:ast:list:empty loc)]
    [(term:surface:list:cons loc first-t rest-t)
     (term:ast:list:cons loc (en first-t) (en rest-t))]
    [(term:surface:text-form loc cmd datums body)
     (define skip (term:surface:list:empty loc))
     (match* (cmd datums body)
       [(#f #f #f)
        (en-error loc "illegal text-form")]
       [((term:surface:id _ '|;|) (not #f) _)
        ;; xxx why bother?
        (en-error loc "illegal comment")]
       [((not #f) #f #f)
        (en cmd)]
       [(_
         (or (as ([datums skip]) #f)
             (term:surface:brackets _ datums))
         (or (as ([body skip]) #f)
             (term:surface:braces _ body)))
        (define inner
          (term:ast:list-append (en datums) (en body)))
        (if cmd
          (term:ast:list:cons loc (en cmd) inner)
          inner)])]
    [(term loc)
     (en-error loc (format "unexpected term: ~e" s))]))

(module+ test
  (define-syntax-rule (check-en i eo)
    (check-en* en i eo))
  (check-en (term:surface:str l0 "test")
            (term:ast:str l0 "test"))
  (check-en (term:surface:id l0 'test)
            (term:ast:id l0 'test))
  (check-en (term:surface:num l0 20)
            (term:ast:num l0 20))
  (check-en (term:surface:op
             l0 (term:surface:id l0 '+))
            (term:ast:id l0 '+))
  (check-en (term:surface:group
             l0 (term:surface:list
                 (term:surface:op l0 (term:surface:id l0 '+))
                 (term:surface:num l0 1)
                 (term:surface:num l0 2)))
            (term:ast:list
             (term:ast:id l0 '+)
             (term:ast:list
              (term:ast:num l0 1)
              (term:ast:num l0 2))))
  (check-en (term:surface:group
             l0 (term:surface:list
                 (term:surface:swap l0 (term:surface:op l0 (term:surface:id l0 '+)))
                 (term:surface:num l0 1)
                 (term:surface:num l0 2)))
            (term:ast:list
             (term:ast:id l0 '+)
             (term:ast:num l0 1)
             (term:ast:num l0 2)))
  (check-en (term:surface:group
             l0 (term:surface:list
                 (term:surface:num l0 1)
                 (term:surface:op l0 (term:surface:id l0 '+))
                 (term:surface:num l0 2)))
            (term:ast:list
             (term:ast:id l0 '+)
             (term:ast:num l0 1)
             (term:ast:num l0 2)))

  (check-en (term:surface:group
             l0 (term:surface:list
                 (term:surface:id l0 'f)
                 (term:surface:num l0 1)
                 (term:surface:op l0 (term:surface:id l0 '+))
                 (term:surface:num l0 2)))
            (term:ast:list
             (term:ast:id l0 '+)
             (term:ast:list
              (term:ast:id l0 'f)
              (term:ast:num l0 1))
             (term:ast:num l0 2)))
  (check-en (term:surface:group
             l0 (term:surface:list
                 (term:surface:id l0 'f)
                 (term:surface:num l0 1)
                 (term:surface:op l0 (term:surface:id l0 '+))
                 (term:surface:num l0 2)
                 (term:surface:op l0 (term:surface:id l0 '*))
                 (term:surface:num l0 3)))
            (term:ast:list
             (term:ast:id l0 '+)
             (term:ast:list
              (term:ast:id l0 'f)
              (term:ast:num l0 1))
             (term:ast:list
              (term:ast:id l0 '*)
              (term:ast:num l0 2)
              (term:ast:num l0 3))))
  ;; xxx parens
  ;; xxx swap
  ;; xxx braces
  ;; xxx brackets
  ;; xxx text
  ;; xxx list - mt / cons
  ;; xxx text-form
  )

;; xxx en tests

(define (en-file s)
  (match-define (term:surface-file loc lang c) s)
  (term:ast-file
   loc (en lang)
   (term:ast:list:cons
    loc (term:ast:id loc '#%module-begin)
    (en c))))

;; xxx en-file tests

(module+ test
  (require racket/runtime-path
           racket/pretty)
  (define-runtime-path examples "../rk/examples")

  (define fip (open-input-file (build-path examples "first.rk")))
  (port-count-lines! fip)
  (pretty-write
   (pp
    (en-file (rd-file fip)))))
