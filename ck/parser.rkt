#lang racket/base
(require racket/list
         racket/match
         unstable/match
         rack/ck/ast
         rack/ck/reader)
(module+ test
  (require rackunit))

(define (loc-merge x y)
  (match-define (srcloc src1 line1 col1 pos1 span1) x)
  (match-define (srcloc src2 line2 col2 pos2 span2) y)
  (srcloc src1 line1 col1 pos1 (- (+ pos2 span2) pos1)))

;; xxx loc-merge

(define str-merge
  (match-lambda
   [(list)
    (list)]
   [(list* (term:ast:str loc1 s1)
           (term:ast:str loc2 s2)
           more)
    (str-merge (term:ast:str (loc-merge loc1 loc2) (string-append s1 s2))
               more)]
   [(list* before after)
    (list* before (str-merge after))]))

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
    [(term:surface:num loc s)
     (term:ast:num loc s)]
    [(term:surface:text loc l)
     (define el (map en l))
     (term:ast:group loc (str-merge el))]
    [(term:surface:text-form loc cmd datums body)
     (define skip (term:ast:group loc empty))
     (match* (cmd datums body)
       [(#f #f #f)
        (en-error loc "illegal text-form")]
       [((term:surface:id _ '|;|) (not #f) _)
        ;; xxx why bother?
        (en-error loc "illegal comment")]
       [((not #f) #f #f)
        cmd]
       [(_
         (or (as ([datums skip]) #f)
             (term:surface:brackets _ datums))
         (or (as ([body skip]) #f)
             (term:surface:braces _ body)))
        (define inner
          (append (term:ast:group-as (en datums))
                  (term:ast:group-as (en body))))
        (if cmd
          (term:ast:group loc (cons (en cmd) inner))
          (term:ast:group loc inner))])]))

;; xxx en tests

(define (en-file s)
  (match-define (term:surface-file loc lang c) s)
  (match-define (term:ast:group ec-loc ec-l) (en c))
  (term:ast-file
   loc (en lang)
   (term:ast:group ec-loc
                   (cons (term:ast:id loc '#%module)
                         ec-l))))

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
      (if (empty? (pre-terms-rators st))
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

  (define (print-prep v)
    (cond
      [(srcloc? v)
       '_]
      [(struct? v)
       (print-prep (struct->vector v))]
      [(vector? v)
       (for/list ([e (in-vector v)]) (print-prep e))]
      [(list? v)
       (for/list ([e (in-list v)]) (print-prep e))]
      [else
       v]))

  (define fip (open-input-file (build-path examples "first.rk")))
  (port-count-lines! fip)
  (pretty-print
   (print-prep
    (en-file (rd-file fip)))))
