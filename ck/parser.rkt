#lang racket/base
(require racket/match
         unstable/match
         rack/ck/ast
         rack/ck/reader)
(module+ test
  (require rackunit))

(define (term:ast:list-append x y)
  (match x
    [(term:ast:list:empty _)
     y]
    [(term:ast:list:cons loc f r)
     (term:ast:list:cons loc f (term:ast:list-append r y))]))

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
      loc1 before (str-merge after))]))

;; xxx str-merge

(define (en-error loc msg)
  (error 'en "~a: ~a" loc msg))

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
     (en s)]
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

;; xxx en tests

(define (en-file s)
  (match-define (term:surface-file loc lang c) s)
  (term:ast-file
   loc (en lang)
   (term:ast:list:cons
    loc (term:ast:id loc '#%module-begin)
    (en c))))

;; xxx en-file tests

(module+ old
  (struct pre-term () #:transparent)
  (struct pre-term:op pre-term (t) #:transparent)

  (define swap-pre-term:op
    (match-lambda
     [(pre-term:op t)
      t]
     [t
      (pre-term:op t)]))

  (struct term () #:transparent)
  (struct term:num term (n) #:transparent)
  (struct term:str term (str) #:transparent)
  (struct term:id term (sym) #:transparent)

  (struct term:comment term (l) #:transparent)
  (struct term:group term (l) #:transparent)
  (struct term:braces term (l) #:transparent)
  (struct term:brackets term (l) #:transparent)

  ;; real code
  '(match* (cmd datums body)
     [('comment #f (term:group l))
      (term:comment l)])

  (struct pre-terms (rands rators) #:transparent)

  (require racket/list)

  ;; xxx at the very least, ; is busted
  (define (parse-rk-terms ip)
    (define (E st)
      (define next (P st))
      (cond
        [(pre-term:op? next)
         (E (push-rator (pre-term:op-t next) st))]
        [next
         (E (push-rand next st))]
        [else
         (pop-rators st)]))
    (define (pop-rators st)
      (if (null? (pre-terms-rators st))
        (reverse (pre-terms-rands st))
        (pop-rators (pop-rator st))))
    (define (pop-rator st)
      (match st
        [(pre-terms (list-rest t1 t0 rands) (list-rest rator rators))
         (pre-terms (list* (term:group (list rator t0 t1)) rands) rators)]
        [(pre-terms (list t0) (list-rest rator rators))
         (pre-terms (list (term:group (list rator t0))) rators)]
        [_
         (error 'pop-rator "can't deal with stack: ~e" st)]))
    (define (push-rand next st)
      (match-define (pre-terms rands rators) st)
      (pre-terms (cons next rands) rators))
    (define (op-precedence o)
      (case (term:id-sym o)
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
        [(|/\\|)
         20]
        [(|\\/|)
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
    (define (top-op st)
      (match-define (pre-terms rands rators) st)
      (match rators
        [(list)
         #f]
        [(list-rest op _)
         op]))
    (define (push-rator op st)
      (cond
        [(op-precedence< op (top-op st))
         (push-rator op (pop-rator st))]
        [else
         (match-define (pre-terms rands rators) st)
         (pre-terms rands (cons op rators))]))
    (define (P st)
      #f)
    (E (pre-terms empty empty))))

(module+ test
  (require racket/runtime-path
           racket/pretty)
  (define-runtime-path examples "../rk/examples")

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
       (cons (pp f) (pp r))]))

  (define fip (open-input-file (build-path examples "first.rk")))
  (port-count-lines! fip)
  (pretty-write
   (pp
    (en-file (rd-file fip)))))
