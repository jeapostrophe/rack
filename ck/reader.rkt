#lang racket/base
(require racket/port
         racket/match
         racket/contract/base
         rack/ck/ast)
(module+ test
  (require rackunit))

(define (read-error ip expected got)
  (error 'read "~a: expected ~v, got ~v, after: ~e"
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
  (when (and (char? c) (? c))
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
    (test-case
     (format "(~a ~v)" 'f is)
     (let ()
       (define ip (open-input-string is))
       (port-count-lines! ip)
       (define ao
         (with-handlers
             ([exn:fail?
               (λ (x)
                 (fail (format "threw error: ~e" (exn-message x))))])
           (f ip)))
       (match ao
         [eop
          (check-equal? #t #t)]
         [_
          (fail (format "~e did not match ~v" ao 'eop))])
       (check-equal? (port->string ip) os))))
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

(define-char-predicate char-closing-forms? ")]}")
(define-char-predicate char-not-in-id? "\"([{}])`;,")

;; XXX take out . : and ->?

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
             (define maybe-size-n
               (and maybe-size (string->number maybe-size)))
             (term:surface:num:int 
              src (string->number (string-append (or maybe-sign "") number-str))
              (and maybe-sign (string->symbol maybe-sign))
              (or maybe-size-n 32)
              maybe-size-n
              #f)])]
       [(regexp-match #rx"^0x([0-9A-Fa-f]+)(i(1|8|16|32|64))?$" s)
        => (match-lambda
            [(list _ number-str _ maybe-size)
             (define maybe-size-n
               (and maybe-size (string->number maybe-size)))
             (term:surface:num:int 
              src (string->number number-str 16)
              #f
              (or maybe-size-n 32)
              maybe-size-n
              16)])]
       [#f ;; xxx
        #rx"^([+-])?([0-9]+\\.[0-9]+|nan\\.0|inf\\.0)(f(16|32|64|80|128))?$"
        #rx"^0b([0-1]+)(i(1|8|16|32|64))?$"
        #rx"^0o([0-7]+)(i(1|8|16|32|64))?$"
        #rx"^0d([0-9]+)(i(1|8|16|32|64))?$"
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
  (check-rd rd-atom "-42" (term:surface:num:int _ -42 '- 32 #f #f) "")
  (check-rd rd-atom  "42" (term:surface:num:int _  42 #f 32 #f #f) "")
  (check-rd rd-atom "+42" (term:surface:num:int _  42 '+ 32 #f #f) "")
  (check-rd rd-atom "-42i32" (term:surface:num:int _ -42 '- 32 32 #f) "")
  (check-rd rd-atom  "42i32" (term:surface:num:int _  42 #f 32 32 #f) "")
  (check-rd rd-atom "+42i32" (term:surface:num:int _  42 '+ 32 32 #f) "")
  (check-rd rd-atom "-42i1" (term:surface:num:int _ -42 '- 1 1 #f) "")
  (check-rd rd-atom  "42i1" (term:surface:num:int _  42 #f 1 1 #f) "")
  (check-rd rd-atom "+42i1" (term:surface:num:int _  42 '+ 1 1 #f) "")
  (check-rd rd-atom "-42i8" (term:surface:num:int _ -42 '- 8 8 #f) "")
  (check-rd rd-atom  "42i8" (term:surface:num:int _  42 #f 8 8 #f) "")
  (check-rd rd-atom "+42i8" (term:surface:num:int _  42 '+ 8 8 #f) "")
  (check-rd rd-atom "-42i16" (term:surface:num:int _ -42 '- 16 16 #f) "")
  (check-rd rd-atom  "42i16" (term:surface:num:int _  42 #f 16 16 #f) "")
  (check-rd rd-atom "+42i16" (term:surface:num:int _  42 '+ 16 16 #f) "")
  (check-rd rd-atom "-42i64" (term:surface:num:int _ -42 '- 64 64 #f) "")
  (check-rd rd-atom  "42i64" (term:surface:num:int _  42 #f 64 64 #f) "")
  (check-rd rd-atom "+42i64" (term:surface:num:int _  42 '+ 64 64 #f) "")
  ;; xxx other numbers
  (check-rd rd-atom "+" (term:surface:op _ (term:surface:id _ '+)) "")
  (check-rd rd-atom "++" (term:surface:op _ (term:surface:id _ '++)) "")
  (check-rd rd-atom "+a+" (term:surface:id _ '+a+) "")
  (check-rd rd-atom "a+" (term:surface:id _ 'a+) "")
  (check-rd rd-atom "a" (term:surface:id _ 'a) "")
  (check-rd rd-atom "#%app" (term:surface:id _ '#%app) "")
  (check-rd rd-atom "#%comment" (term:surface:id _ '#%comment) "")
  (check-rd rd-atom "#%brackets" (term:surface:id _ '#%brackets) "")
  (check-rd rd-atom "#%braces" (term:surface:id _ '#%braces) "")
  (check-rd rd-atom "#%module" (term:surface:id _ '#%module) "")
  (check-rd rd-atom "a," (term:surface:id _ 'a) ",")
  (check-rd rd-atom "a;" (term:surface:id _ 'a) ";")
  (check-rd rd-atom "a`" (term:surface:id _ 'a) "`")
  (check-rd-err rd-atom "+4a")
  (check-rd-err rd-atom "-4a")
  (check-rd-err rd-atom "4a"))

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
     (rd-wrapped ip "`" "`" rd-term term:surface:swap)]
    [(or (? eof-object?)
         (? char-closing-forms?)
         (? char-whitespace?))
     #f]
    [_
     (rd-atom ip)]))

(module+ test
  (check-rd rd*-term "" #f "")
  (check-rd rd*-term " more" #f " more")
  (check-rd rd*-term "\"foo\"bar"
            (term:surface:str _ "foo") "bar")
  (check-rd rd*-term "@foo bar"
            (term:surface:text-form _ (term:surface:id _ 'foo) #f #f) " bar")
  (check-rd rd*-term "(foo bar)zog"
            (term:surface:parens
             _
             (term:surface:group
              _ (term:surface:list:cons
                 _ (term:surface:id _ 'foo)
                 (term:surface:list:cons
                  _ (term:surface:id _ 'bar)
                  (term:surface:list:empty _)))))
            "zog")
  (check-rd rd*-term "{foo bar}zog"
            (term:surface:braces
             _
             (term:surface:group
              _ (term:surface:list:cons
                 _ (term:surface:id _ 'foo)
                 (term:surface:list:cons
                  _ (term:surface:id _ 'bar)
                  (term:surface:list:empty _)))))
            "zog")
  (check-rd rd*-term "[foo bar]zog"
            (term:surface:brackets
             _
             (term:surface:group
              _ (term:surface:list:cons
                 _ (term:surface:id _ 'foo)
                 (term:surface:list:cons
                  _ (term:surface:id _ 'bar)
                  (term:surface:list:empty _)))))
            "zog")
  (check-rd rd*-term "`foo`zog"
            (term:surface:swap _ (term:surface:id _ 'foo)) "zog")
  (check-rd rd*-term "``foo``zog"
            (term:surface:swap _ (term:surface:swap _ (term:surface:id _ 'foo)))
            "zog")
  (check-rd rd*-term "`(foo bar)`zog"
            (term:surface:swap
             _
             (term:surface:parens
              _
              (term:surface:group
               _ (term:surface:list:cons
                  _ (term:surface:id _ 'foo)
                  (term:surface:list:cons
                   _ (term:surface:id _ 'bar)
                   (term:surface:list:empty _))))))
            "zog")
  (check-rd rd*-term ")bar" #f ")bar")
  (check-rd rd*-term "]bar" #f "]bar")
  (check-rd rd*-term "}bar" #f "}bar")
  (check-rd rd*-term "foo" (term:surface:id _ 'foo) ""))

(define (rd-term ip)
  (or (rd*-term ip)
      (read-error ip "term" "nothing")))

(module+ test
  (check-rd-err rd-term "")
  (check-rd-err rd-term " more")
  (check-rd-err rd-term ")bar")
  (check-rd-err rd-term "]bar")
  (check-rd-err rd-term "}bar")
  (check-rd rd-term "foo" (term:surface:id _ 'foo) ""))

(define (rd-terms ip)
  (define start (port-location ip))
  (define l
    (let loop ()
      (define start (port-location ip))
      (slurp-whitespace ip)
      (define first-t (rd*-term ip))
      (cond
        [first-t
         (define rest-ts (loop))
         (term:surface:list:cons (locs->srcloc start ip)
                                 first-t
                                 rest-ts)]
        [else
         (term:surface:list:empty (locs->srcloc start ip))])))
  (term:surface:group (locs->srcloc start ip) l))

(module+ test
  (check-rd rd-terms "" (term:surface:group
                         _ (term:surface:list:empty _)) "")
  (check-rd rd-terms "]" (term:surface:group
                          _ (term:surface:list:empty _)) "]")
  (check-rd rd-terms ")" (term:surface:group
                          _ (term:surface:list:empty _)) ")")
  (check-rd rd-terms "}" (term:surface:group
                          _ (term:surface:list:empty _)) "}")
  (check-rd rd-terms "foo}"
            (term:surface:group
             _ (term:surface:list:cons
                _ (term:surface:id _ 'foo)
                (term:surface:list:empty _)))
            "}")
  (check-rd rd-terms "foo bar}"
            (term:surface:group
             _ (term:surface:list:cons
                _ (term:surface:id _ 'foo)
                (term:surface:list:cons
                 _ (term:surface:id _ 'bar)
                 (term:surface:list:empty _))))
            "}")
  (check-rd rd-terms "[foo]}"
            (term:surface:group
             _ (term:surface:list:cons
                _ (term:surface:brackets
                   _ (term:surface:group
                      _ (term:surface:list:cons
                         _ (term:surface:id _ 'foo)
                         (term:surface:list:empty _))))
                (term:surface:list:empty _)))
            "}")
  (check-rd rd-terms "(foo)}"
            (term:surface:group
             _ (term:surface:list:cons
                _ (term:surface:parens
                   _ (term:surface:group
                      _ (term:surface:list:cons
                         _ (term:surface:id _ 'foo)
                         (term:surface:list:empty _))))
                (term:surface:list:empty _)))
            "}")
  (check-rd rd-terms "{foo}}"
            (term:surface:group
             _ (term:surface:list:cons
                _ (term:surface:braces
                   _ (term:surface:group
                      _ (term:surface:list:cons
                         _ (term:surface:id _ 'foo)
                         (term:surface:list:empty _))))
                (term:surface:list:empty _)))
            "}"))

(define (rd*-text-cmd ip)
  (match (peek-char ip)
    [(or #\[ #\{)
     #f]
    [_
     (rd*-term ip)]))

(module+ test
  (check-rd rd*-text-cmd "foo[]{}" (term:surface:id _ 'foo) "[]{}")
  (check-rd rd*-text-cmd "(foo)[]{}"
            (term:surface:parens
             _ (term:surface:group
                _ (term:surface:list:cons
                   _ (term:surface:id _ 'foo)
                   (term:surface:list:empty _)))) "[]{}")
  (check-rd rd*-text-cmd "[foo]{}" #f "[foo]{}")
  (check-rd rd*-text-cmd "{foo}" #f "{foo}"))

(define (rd*-text-datums ip)
  (match (peek-char ip)
    [#\[
     (rd-wrapped ip "[" "]" rd-terms term:surface:brackets)]
    [_
     #f]))

(module+ test
  (check-rd rd*-text-datums "{foo}" #f "{foo}")
  (check-rd rd*-text-datums "[foo]{bar}"
            (term:surface:brackets
             _ (term:surface:group
                _ (term:surface:list:cons
                   _ (term:surface:id _ 'foo)
                   (term:surface:list:empty _))))
            "{bar}"))

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
           (define c-start (port-location ip))
           (define f (rd-text-form ip))
           (define r (loop))
           (term:surface:list:cons (locs->srcloc c-start ip) f r)]
          [(or #\} (? eof-object?))
           (define e-start (port-location ip))
           (term:surface:list:empty (locs->srcloc e-start ip))]
          [x
           (read-error ip "@, }, or eof" x)]))
      (if (zero? len)
        v
        (term:surface:list:cons
         (locs->srcloc a-start ip)
         (term:surface:str a-src str)
         v))))
  (define src (locs->srcloc start ip))
  (term:surface:text src result))

(define (rd-text-inner ip)
  (rd-text ip #t))

(module+ test
  (check-rd rd-text-inner "foo bar baz"
            (term:surface:text
             _ (term:surface:list:cons
                _ (term:surface:str _ "foo bar baz")
                (term:surface:list:empty _)))
            "")
  (check-rd rd-text-inner "foo bar}baz"
            (term:surface:text
             _ (term:surface:list:cons
                _ (term:surface:str _ "foo bar")
                (term:surface:list:empty _)))
            "}baz")
  (check-rd rd-text-inner "foo @zog bar}baz"
            (term:surface:text
             _
             (term:surface:list:cons
              _ (term:surface:str _ "foo ")
              (term:surface:list:cons
               _ (term:surface:text-form
                  _
                  (term:surface:id _ 'zog)
                  #f
                  #f)
               (term:surface:list:cons
                _ (term:surface:str _ " bar")
                (term:surface:list:empty _)))))
            "}baz"))

(define (rd*-text-body ip)
  (match (peek-char ip)
    [#\{
     (rd-wrapped ip "{" "}" rd-text-inner term:surface:braces)]
    [_
     #f]))

(module+ test
  (check-rd rd*-text-body "@foo" #f "@foo")
  (check-rd rd*-text-body "{foo bar baz}"
            (term:surface:braces
             _ (term:surface:text
                _ (term:surface:list:cons
                   _ (term:surface:str _ "foo bar baz")
                   (term:surface:list:empty _)))) "")
  (check-rd rd*-text-body "{foo bar}baz"
            (term:surface:braces
             _ (term:surface:text
                _ (term:surface:list:cons
                   _ (term:surface:str _ "foo bar")
                   (term:surface:list:empty _))))
            "baz")
  (check-rd rd*-text-body "{foo @zog bar}baz"
            (term:surface:braces
             _
             (term:surface:text
              _ (term:surface:list:cons
                 _ (term:surface:str _ "foo ")
                 (term:surface:list:cons
                  _ (term:surface:text-form
                     _
                     (term:surface:id _ 'zog)
                     #f
                     #f)
                  (term:surface:list:cons
                   _ (term:surface:str _ " bar")
                   (term:surface:list:empty _))))))
            "baz"))

(define (rd-text-form ip)
  (define start (port-location ip))
  (expect ip "@")
  (define cmd (rd*-text-cmd ip))
  (define datums (rd*-text-datums ip))
  (define body (rd*-text-body ip))
  (term:surface:text-form (locs->srcloc start ip) cmd datums body))

(module+ test
  (check-rd rd-text-form "@foo[bar]{zog}more"
            (term:surface:text-form
             _ (term:surface:id _ 'foo)
             (term:surface:brackets
              _ (term:surface:group
                 _ (term:surface:list:cons
                    _ (term:surface:id _ 'bar)
                    (term:surface:list:empty _))))
             (term:surface:braces
              _ (term:surface:text
                 _ (term:surface:list:cons
                    _ (term:surface:str _ "zog")
                    (term:surface:list:empty _)))))
            "more")
  (check-rd rd-text-form "@foo[bar]more"
            (term:surface:text-form
             _ (term:surface:id _ 'foo)
             (term:surface:brackets
              _ (term:surface:group
                 _ (term:surface:list:cons
                    _ (term:surface:id _ 'bar)
                    (term:surface:list:empty _))))
             #f)
            "more")
  (check-rd rd-text-form "@[bar]more"
            (term:surface:text-form
             _ #f
             (term:surface:brackets
              _ (term:surface:group
                 _ (term:surface:list:cons
                    _ (term:surface:id _ 'bar)
                    (term:surface:list:empty _))))
             #f)
            "more")
  (check-rd rd-text-form "@[bar]{zog}more"
            (term:surface:text-form
             _ #f
             (term:surface:brackets
              _ (term:surface:group
                 _ (term:surface:list:cons
                    _ (term:surface:id _ 'bar)
                    (term:surface:list:empty _))))
             (term:surface:braces
              _ (term:surface:text
                 _ (term:surface:list:cons
                    _ (term:surface:str _ "zog")
                    (term:surface:list:empty _)))))
            "more")
  (check-rd rd-text-form "@{zog}more"
            (term:surface:text-form
             _ #f
             #f
             (term:surface:braces
              _ (term:surface:text
                 _ (term:surface:list:cons
                    _ (term:surface:str _ "zog")
                    (term:surface:list:empty _)))))
            "more")
  (check-rd rd-text-form "@foo[bar] {zog}more"
            (term:surface:text-form
             _ (term:surface:id _ 'foo)
             (term:surface:brackets
              _ (term:surface:group
                 _ (term:surface:list:cons
                    _ (term:surface:id _ 'bar)
                    (term:surface:list:empty _))))
             #f)
            " {zog}more")
  (check-rd rd-text-form "@foo [bar]{zog}more"
            (term:surface:text-form
             _ (term:surface:id _ 'foo)
             #f
             #f)
            " [bar]{zog}more")
  (check-rd rd-text-form "@ foo[bar]{zog}more"
            (term:surface:text-form
             _ #f
             #f
             #f)
            " foo[bar]{zog}more"))

(define (rd-text-outer ip)
  (rd-text ip #f))

(module+ test
  (check-rd rd-text-outer "foo bar baz"
            (term:surface:text
             _ (term:surface:list:cons
                _ (term:surface:str _ "foo bar baz")
                (term:surface:list:empty _)))
            "")
  (check-rd rd-text-outer "foo bar}baz"
            (term:surface:text
             _ (term:surface:list:cons
                _ (term:surface:str _ "foo bar}baz")
                (term:surface:list:empty _)))
            "")
  (check-rd rd-text-outer "foo @zog bar}baz"
            (term:surface:text
             _ (term:surface:list:cons
                _ (term:surface:str _ "foo ")
                (term:surface:list:cons
                 _ (term:surface:text-form
                    _
                    (term:surface:id _ 'zog)
                    #f
                    #f)
                 (term:surface:list:cons
                  _ (term:surface:str _ " bar}baz")
                  (term:surface:list:empty _)))))
            ""))

(define (rd-file ip)
  (define file-start (port-location ip))
  (expect ip "#lang")
  (expect-char ip char-whitespace?)
  (slurp-whitespace ip)
  (define lang-start (port-location ip))
  (expect ip "rk")
  (define lang
    (term:surface:str (locs->srcloc lang-start ip) "rk"))
  (expect-char ip char-whitespace?)
  (define content
    (rd-text-outer ip))
  (term:surface-file (locs->srcloc file-start ip) lang content))

(module+ test
  (check-rd rd-file "#lang rk foo"
            (term:surface-file
             _ (term:surface:str _ "rk")
             (term:surface:text
              _ (term:surface:list:cons
                 _ (term:surface:str _ "foo")
                 (term:surface:list:empty _))))
            "")
  (check-rd-err rd-file "#langrk foo")
  (check-rd-err rd-file "#lang rkfoo")
  (check-rd-err rd-file "#lang rkt foo"))

(define rd-body rd-text-outer)

(provide
 (contract-out
  [rd-file 
   (-> input-port?
       term:surface-file?)]
  [rd-body
   (-> input-port?
       term:surface:text?)]))
