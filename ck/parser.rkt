#lang racket/base
(require racket/port
         racket/list
         racket/match
         rack/ck/ast
         rack/ck/reader)
(module+ test
  (require rackunit))

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
  (define (merging-cons v l)
    (match* (v l)
      [((term:str s1) (cons (term:str s2) l))
       (cons (term:str (string-append s1 s2)) l)]
      [(_ _)
       (cons v l)]))

  '(match* (cmd datums body)
     [('comment #f (term:group l))
      (term:comment l)]
     [((not #f) #f (term:group l))
      (term:group (cons cmd l))]
     [((not #f) (list d ...) (term:group l))
      (term:group (cons cmd (append d l)))]
     [((not #f) (list d ...) #f)
      (term:group (cons cmd d))]
     [((not #f) #f #f)
      cmd]
     [(#f #f (term:group l))
      body]
     [(_ _ _)
      (error 'parse-text-form "invalid text-form: ~v"
             (vector cmd datums body))])

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
  (pretty-print (print-prep (rd-file fip))))
