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

;; ast

(struct term () #:transparent)
(struct term:num term (n) #:transparent)
(struct term:str term (str) #:transparent)
(struct term:id term (sym) #:transparent)
(struct term:op term (sym) #:transparent)
(struct term:keyword term (sym) #:transparent)

(struct term:comment term (l) #:transparent)
(struct term:group term (l) #:transparent)
(struct term:braces term (l) #:transparent)
(struct term:brackets term (l) #:transparent)

;; real code

(define (slurp-whitespace ip)
  (read-until-not ip char-whitespace?))

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
     (eprintf "got ~v\n" str)
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

(define (maybe-parse-rk-term ip)
  (match (peek-char ip)
    [#\#
     (parse-rk-hash ip)]
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
    ;; xxx backticks
    [(? char-initial-operator?)
     (parse-rk-operator ip)]
    [(? char-initial-id?)
     (parse-rk-id ip)]
    [(? char-numeric?)
     (parse-rk-num ip)]
    [_
     #f]))

;; xxx
(define (parse-rk-num ip)
  (define len (count-while ip char-numeric?))
  (define s (read-string len ip))
  (term:num (string->number s)))

(define (parse-rk-string ip)
  (expect ip "\"")
  (define len (count-until ip (λ (c) (char=? #\" c))))
  (define s (read-string len ip))
  (expect ip "\"")
  (term:str s))

(define id-enders "{}[]@()\"\"")

(define (char-id-ender? c)
  (ormap (λ (c1) (char=? c c1))
         (string->list id-enders)))

;; xxx
(define (char-initial-operator? c)
  (or (char=? c #\;)
      (char=? c #\,)
      (member (char-general-category c) '(sm))))

(define (parse-rk-operator ip)
  (define len
    (count-until ip
                 (λ (c)
                   (or (char-whitespace? c)
                       (char-id-ender? c)
                       (char-iso-control? c)))))
  (when (zero? len)
    (error 'parse-rk-id "empty op: ~e" (port->string ip)))
  (define s (read-string len ip))
  (term:op (string->symbol s)))

;; xxx
(define (char-initial-id? c)
  (not
   (or (char-whitespace? c)
       (char-numeric? c)
       (char-iso-control? c)
       (char-id-ender? c))))

(define (parse-rk-id ip)
  (define len
    (count-until ip
                 (λ (c) (or (char-whitespace? c)
                            (char=? c #\;)
                            (char=? c #\,)
                            (char-id-ender? c)
                            (char-iso-control? c)))))
  (when (zero? len)
    (error 'parse-rk-id "empty id: ~e" (port->string ip)))
  (term:id (string->symbol (read-string len ip))))

(define (parse-rk-hash ip)
  (expect ip "#")
  (match (peek-char ip)
    [#\:
     (expect ip ":")
     (define s (parse-rk-id ip))
     (unless s
       (error 'parse-rk-hash "expected id after #:, got ~v" s))
     (term:keyword s)]
    [_
     (error 'parse-rk-hash "~v" (port->string ip))]))

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

(define (parse-rk-terms ip)
  (let loop ([stack empty])
    (slurp-whitespace ip)
    (match (peek-char ip)
      ;; xxx implement operator precedence and associativity
      [_
       (define t (maybe-parse-rk-term ip))
       (eprintf "prt ~v\n" t)
       (if t
         (loop (cons t stack))
         (reverse stack))])))

(define-syntax-rule (define-undefined id)
  (define (id . args)
    (error 'id "unimplemented: ~v"
           (map
            (λ (x)
              (if (input-port? x)
                (port->string x)
                x))
            args))))

(module+ test
  (require racket/runtime-path
           racket/pretty)
  (define-runtime-path examples "../rk/examples")

  (pretty-print
   (parse-rk-file
    (open-input-file (build-path examples "first.rk")))))
