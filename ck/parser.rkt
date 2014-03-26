#lang racket/base
(require racket/port
         racket/list
         racket/match
         rack/ck/ast)
(module+ test
  (require rackunit))

(define (read-error ip expected got)
  (error 'read "~a: expected ~v, got ~v, after: ~e\n"
         (port-location ip)
         expected got
         (port->string ip)))

(define (expect-char ip ?)
  (define c (peek-char ip))
  (if (? c)
    (read-char ip)
    (read-error ip ? c)))

(define (expect ip str)
  (define len (string-length str))
  (define actual (peek-string len 0 ip))
  (if (string=? str actual)
    (read-string len ip)
    (read-error ip str actual)))

(define (read-until-not ip ?)
  (define c (peek-char ip))
  (when (? c)
    (read-char ip)
    (read-until-not ip ?)))

(define (count-until ip ?)
  (let loop ([i 0] [bs 0])
    (define c (peek-char ip bs))
    (cond
      [(eof-object? c)
       i]
      [(not (? c))
       (loop (add1 i) (+ bs (char-utf-8-length c)))]
      [else
       i])))

(define (count-while ip ?)
  (count-until ip (λ (c) (not (? c)))))

(define (slurp-whitespace ip)
  (read-until-not ip char-whitespace?))

;; Parser structure

;; input-port -(rd)> surface -(en)> ast

(define (port-location ip)
  (call-with-values (λ () (port-next-location ip)) vector))
(define (locs->srcloc start ip)
  (match-define (vector line column start-pos) start)
  (define end (port-location ip))
  (match-define (vector _ _ end-pos) end)
  (srcloc (object-name ip)
          line column start-pos
          (- end-pos start-pos)))

(module+ test
  (define-syntax-rule (check-rd f is eop os)
    (let ()
      (define ip (open-input-string is))
      (port-count-lines! ip)
      (define ao (f ip))
      (match ao
        [eop
         (check-equal? #t #t)]
        [_
         (check-equal? #t #f)])
      (check-equal? (port->string ip) os)))
  (define-syntax-rule (check-rd-err f is)
    (let ()
      (define ip (open-input-string is))
      (port-count-lines! ip)
      (check-exn exn:fail? (λ () (f ip))))))

(define (rd-wrapped ip before after inner k)
  (define start (port-location ip))
  (expect ip before)
  (define v (inner ip))
  (expect ip after)
  (define src (locs->srcloc start ip))
  (k src v))

(define (rd-string ip)
  (rd-wrapped
   ip "\"" "\""
   (λ (ip)
     (define len (count-until ip (λ (c) (char=? #\" c))))
     (read-string len ip))
   term:surface:str))

(module+ test
  (check-rd rd-string "\"foo\"bar" (term:surface:str _ "foo") "bar")
  (check-rd rd-string "\"fo\\\"o\"bar" (term:surface:str _ "fo\\") "o\"bar"))

(define-syntax-rule (define-char-predicate id str)
  (begin (define cs (string->list str))
         (define (id c) (member c cs))))

(define-char-predicate char-closing-forms? ")]}`")
(define-char-predicate char-not-in-id? "\"([{}])`;,")

(define (rd-atom ip)
  (define start (port-location ip))
  (define len
    (count-until
     ip
     (λ (c) (or (char-whitespace? c)
                (char-iso-control? c)
                (char-not-in-id? c)))))
  (cond
    [(zero? len)
     (define s
       (match (peek-char ip)
         [#\; '|;|]
         [#\, '|,|]
         [x
          (read-error ip "special atom" x)]))
     (read-char ip)
     (define src (locs->srcloc start ip))
     (term:surface:op src (term:surface:id src s))]
    [else
     (define s (read-string len ip))
     (define src (locs->srcloc start ip))
     (define sl (string->list s))
     (cond
       ;;     sign = + | -
       ;; 0b-digit = {0 1}
       ;; 0o-digit = {0 1 2 3 4 5 6 7}
       ;; 0d-digit = {0 1 2 3 4 5 6 7 8 9}
       ;; 0x-digit = {0 1 2 3 4 5 6 7 8 9 a A b B c C d D e E f F}
       ;;  isuffix = i{ 1  8 16 32  64}
       ;;  fsuffix = f{16 32 64 80 128}
       ;;   period = .
       ;;   prefix = 0b | 0o | 0d | 0x
       ;;      num = sign? 0d-digit+ period 0d-digit+ fsuffix?
       ;;          | sign? 0d-digit+ isuffix?
       ;;          | prefix prefix-digit+ isuffix?
       [(regexp-match #rx"^([+-])?([0-9]+)(i(1|8|16|32|64))?$" s)
        => (match-lambda
            [(list _ maybe-sign number-str _ maybe-size)
             ;; xxx sign and size
             (term:surface:num src (string->number number-str))])]
       [#f ;; xxx
        #rx"^([+-])?([0-9]+\\.[0-9]+|nan\\.0|inf\\.0)(f(16|32|64|80|128))?$"
        #rx"^0b([0-1]+)(i(1|8|16|32|64))?$"
        #rx"^0o([0-7]+)(i(1|8|16|32|64))?$"
        #rx"^0d([0-9]+)(i(1|8|16|32|64))?$"
        #rx"^0x([0-9A-Fa-f]+)(i(1|8|16|32|64))?$"
        ;; xxx read single unicode character as a number
        #rx"^0u(.)(i(1|8|16|32|64))?$"]
       ;; An operator contains no numbers or alphabetic characters
       [(not
         (ormap (λ (c) (or (char-alphabetic? c)
                           (char-numeric? c)))
                sl))
        (term:surface:op src (term:surface:id src (string->symbol s)))]
       ;; An identifier doesn't start with a ([+-]?[0-9]) to prevent
       [(not (regexp-match #rx"^[+-]?[0-9]" s))
        (term:surface:id src (string->symbol s))]
       [else
        (read-error ip "atom" s)])]))

(module+ test
  (check-rd rd-atom ";" (term:surface:op _ (term:surface:id _ '|;|)) "")
  (check-rd rd-atom "," (term:surface:op _ (term:surface:id _ '|,|)) "")
  ;; xxx sign and size
  (check-rd rd-atom "42" (term:surface:num _ 42) "")
  (check-rd rd-atom "+42" (term:surface:num _ 42) "")
  ;; xxx other numbers
  (check-rd rd-atom "+" (term:surface:op _ (term:surface:id _ '+)) "")
  (check-rd rd-atom "++" (term:surface:op _ (term:surface:id _ '++)) "")
  (check-rd rd-atom "+a+" (term:surface:id _ '+a+) "")
  (check-rd rd-atom "a+" (term:surface:id _ 'a+) "")
  (check-rd rd-atom "a" (term:surface:id _ 'a) "")
  (check-rd rd-atom "a," (term:surface:id _ 'a) ",")
  (check-rd rd-atom "a;" (term:surface:id _ 'a) ";")
  (check-rd-err rd-atom "+4a")
  (check-rd-err rd-atom "-4a")
  (check-rd-err rd-atom "4a"))

(define (rd-term ip)
  (or (rd*-term ip)
      (read-error ip "term" "nothing")))

;; xxx rd-term tests

(define (rd*-term ip)
  (match (peek-char ip)
    [#\"
     (rd-string ip)]
    [#\@
     (rd-text-form ip)]
    [#\(
     (rd-wrapped ip "(" ")" rd-terms term:surface:parens)]
    [#\{
     (rd-wrapped ip "{" "}" rd-terms term:surface:braces)]
    [#\[
     (rd-wrapped ip "[" "]" rd-terms term:surface:brackets)]
    [#\`
     ;; xxx why term and not terms here?
     (rd-wrapped ip "`" "`" rd-term term:surface:swap)]
    [(? char-closing-forms?)
     #f]
    [_
     (rd-atom ip)]))

;; xxx rd*-term tests

(define (rd-terms ip)
  (define start (port-location ip))
  (define result
    (let loop ()
      (slurp-whitespace ip)
      (define v (rd*-term ip))
      (if v
        (cons v (loop))
        empty)))
  (term:surface:group (locs->srcloc start ip) result))

;; xxx rd-terms tests

(define (rd-text-form ip)
  (define start (port-location ip))
  (expect ip "@")
  (define cmd (rd*-text-cmd ip))
  (define datums (rd*-text-datums ip))
  (define body (rd*-text-body ip))
  (term:surface:text-form (locs->srcloc start ip) cmd datums body))

;; xxx rd-text-form

(define (rd*-text-cmd ip)
  (match (peek-char ip)
    [(or #\[ #\{)
     #f]
    [_
     (rd*-term ip)]))

;; xxx rd*-text-cmd

(define (rd*-text-datums ip)
  (match (peek-char ip)
    [#\[
     (rd-wrapped ip "[" "]" rd-terms term:surface:brackets)]
    [_
     #f]))

;; xxx rd*-text-datums

(define (rd*-text-body ip)
  (match (peek-char ip)
    [#\{
     (rd-wrapped ip "{" "}" rd-terms term:surface:braces)]
    [_
     #f]))

;; xxx rd*-text-body

(define (rd-file ip)
  (and
   (expect ip "#lang")
   (slurp-whitespace ip)
   (expect ip "rk")
   (expect-char ip char-whitespace?)
   (rd-text-outer ip)))

;; xxx rd-file

(define (rd-text-outer ip)
  (rd-text ip #f))
(define (rd-text-inner ip)
  (rd-text ip #t))

(define (rd-text ip inside?)
  (define start (port-location ip))
  (define result
    (let loop ()
      (define a-start (port-location ip))
      (define len
        (count-until
         ip
         (λ (c)
           (or (char=? c #\@)
               (if inside?
                 (char=? c #\})
                 #f)))))
      (define str (read-string len ip))
      (define a-src (locs->srcloc a-start ip))
      (define v
        (match (peek-char ip)
          [#\@
           (cons (rd-text-form ip) (loop))]
          [(or #\} (? eof-object?))
           empty]
          [x
           (read-error ip "@, }, or eof" x)]))
      (if (zero? len)
        v
        (cons (term:surface:str a-src str) v))))
  (define src (locs->srcloc start ip))
  (term:surface:text src result))

;; xxx rd-text-outer
;; xxx rd-text-inner

;; ast

;; xxx modify ast to account for details of how the program was really
;; written (i.e. the presence of unnecessary ()s, ``s, and @) so that
;; it can be "reprinted"

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
      (slurp-whitespace ip)
      (rd*-term ip))
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
