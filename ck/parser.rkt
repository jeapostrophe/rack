#lang racket/base
(require racket/port
         racket/list
         racket/match)

(define (expect-char ip ?)
  (define c (peek-char ip))
  (if (? c)
    (read-char ip)
    (error 'expect-char "expected ~v, got ~v, then ~e\n" ? c (port->string ip))))

(define (expect ip str)
  (define len (string-length str))
  (define actual (peek-string len 0 ip))
  (if (string=? str actual)
    (read-string len ip)
    (error 'expect "expected ~v, got ~v, then ~e\n" str actual (port->string ip))))

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

;; ast

;; xxx modify ast to account for details of how the program was really
;; written (i.e. the presence of unnecessary ()s, ``s, and @) so that
;; it can be "reprinted"

;; xxx implement testing system

;; xxx implement srcloc system

(struct pre-term () #:transparent)
(struct pre-term:op pre-term (t) #:transparent)

(define un-pre-term:op
  (match-lambda
   [(pre-term:op t)
    t]
   [t
    t]))

(struct term () #:transparent)
(struct term:num term (n) #:transparent)
(struct term:str term (str) #:transparent)
(struct term:id term (sym) #:transparent)

(struct term:comment term (l) #:transparent)
(struct term:group term (l) #:transparent)
(struct term:braces term (l) #:transparent)
(struct term:brackets term (l) #:transparent)

;; real code

(define (parse-rk-file ip)
  (and
   (expect ip "#lang")
   (slurp-whitespace ip)
   (expect ip "rk")
   (expect-char ip char-whitespace?)
   (parse-outer-text ip)))

(define (parse-outer-text ip)
  (parse-text ip #f))
(define (parse-inner-text ip)
  (parse-text ip #t))

(define (merging-cons v l)
  (match* (v l)
    [((term:str s1) (cons (term:str s2) l))
     (cons (term:str (string-append s1 s2)) l)]
    [(_ _)
     (cons v l)]))

(define (parse-text ip inside?)
  (term:group
   (let loop ()
     (define len
       (count-until
        ip
        (λ (c)
          (or (char=? c #\@)
              (if inside?
                (char=? c #\})
                #f)))))
     (define str (read-string len ip))
     (define (kontA v)
       (if (zero? len)
         v
         (merging-cons (term:str str) v)))
     (match (peek-char ip)
       [#\@
        (kontA
         (merging-cons (parse-text-form ip) (loop)))]
       [#\}
        (kontA empty)]
       [(? eof-object?)
        (kontA empty)]
       [x
        (error 'parse-text "got ~v, then unexpected ~e, then ~v"
               str x (port->string ip))]))))

(define (parse-text-form ip)
  (expect ip "@")
  (define cmd (maybe-parse-text-cmd ip))
  (define datums (maybe-parse-text-datums ip))
  (define body (maybe-parse-text-body ip))
  (match* (cmd datums body)
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
            (vector cmd datums body))]))

(define (maybe-parse-text-cmd ip)
  (match (peek-char ip)
    [#\;
     (read-char ip)
     'comment]
    [(or #\[ #\{)
     #f]
    [_
     (maybe-parse-rk-term ip)]))

(define-syntax-rule (define-char-predicate id str)
  (begin (define cs (string->list str))
         (define (id c) (member c cs))))

(define-char-predicate char-closing-forms? ")]}`|")
(define-char-predicate char-not-in-id? "\"([{}])`;,|")

(define (parse-rk-term ip)
  (define v (maybe-parse-rk-term ip))
  (unless v
    (error 'parse-rk-term "expected rk-term at ~e" (port->string ip)))
  v)

(define (maybe-parse-rk-term ip)
  (match (peek-char ip)
    [#\"
     (parse-rk-string ip)]
    [#\@
     (parse-text-form ip)]
    [#\(
     (expect ip "(")
     (begin0 (term:group (parse-rk-terms ip))
             (expect ip ")"))]
    [#\{
     (expect ip "{")
     (begin0 (term:braces (parse-rk-terms ip))
             (expect ip "}"))]
    [#\[
     (expect ip "[")
     (begin0 (term:brackets (parse-rk-terms ip))
             (expect ip "]"))]
    [#\`
     (expect ip "`")
     (begin0 (pre-term:op (parse-rk-term ip))
             (expect ip "`"))]
    [#\|
     (expect ip "|")
     (begin0 (un-pre-term:op (parse-rk-term ip))
             (expect ip "|"))]
    [(? char-closing-forms?)
     #f]
    [_
     (parse-rk-atom ip)]))

(define (parse-rk-atom ip)
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
         [_
          (error 'parse-rk-atom "expected a special symbol, got ~e"
                 (port->string ip))]))
     (read-char ip)
     (pre-term:op (term:id s))]
    [else
     (define s (read-string len ip))
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
             (term:num (string->number number-str))])]
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
        (pre-term:op (term:id (string->symbol s)))]
       ;; An identifier doesn't start with a ([+-]?[0-9]) to prevent
       [(not (regexp-match #rx"^[+-]?[0-9]" s))
        (term:id (string->symbol s))]
       [else
        (error 'parse-rk-atom "lost at ~e" s)])]))

(define (parse-rk-string ip)
  (expect ip "\"")
  (define len (count-until ip (λ (c) (char=? #\" c))))
  (define s (read-string len ip))
  (expect ip "\"")
  (term:str s))

(define (maybe-parse-text-datums ip)
  (match (peek-char ip)
    [#\[
     (expect ip "[")
     (begin0 (parse-rk-terms ip)
             (expect ip "]"))]
    [_
     #f]))

(define (maybe-parse-text-body ip)
  (match (peek-char ip)
    [#\{
     (expect ip "{")
     (define t (parse-inner-text ip))
     (expect ip "}")
     t]
    [_
     #f]))

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
    (maybe-parse-rk-term ip))
  (E (pre-terms empty empty)))

(module+ test
  (require racket/runtime-path
           racket/pretty)
  (define-runtime-path examples "../rk/examples")

  (pretty-print
   (parse-rk-file
    (open-input-file (build-path examples "first.rk")))))
