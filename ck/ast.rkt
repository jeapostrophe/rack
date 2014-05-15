#lang racket/base
(require racket/contract/base
         rack/etc/define-types)

(define-types
  [(term [src srcloc?])
   
   [(surface-file
     [lang term:surface:str?]
     [content term:surface?])]
   [surface
    [(op [t term:surface?])]
    [list
     [empty]
     [(cons [first term:surface?]
            [rest term:surface:list?])]]
    [(group [l term:surface:list?])]
    [(swap [t term:surface?])]
    [(unary-op [t term:surface?])]
    [(parens [t term:surface?])]
    [(braces [t term:surface?])]
    [(brackets [t term:surface?])]
    [(text [t term:surface?])]
    [(text-form [cmd (or/c #f term:surface?)]
                [datums (or/c #f term:surface:brackets?)]
                [body (or/c #f term:surface:braces?)])]

    [(str [s string?])]
    [(id [s symbol?])]
    ;; xxx be more specific
    [num
     [(int [n number?]
           [sign-stx (or/c #f '+ '-)]
           [size-act number?]
           [size-stx (or/c #f number?)]
           [base (or/c #f 2 8 10 16 'unicode)])]]]

   [(ast-file
     [lang term:ast:str?]
     [content term:ast?])]
   [ast
    [list
     [empty]
     [(cons [first term:ast?]
            [rest term:ast:list?])]]

    [(str [s string?])]
    [(id [s symbol?])]
    ;; xxx be more specific
    [num
     [(int [n number?]
           [size number?])]]]])
