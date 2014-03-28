#lang racket/base
(require racket/contract/base
         rack/etc/define-types)

(define-types
  [(term [src srcloc?])
   [surface
    [(op [t term:surface?])]
    [(group [t (listof term:surface?)])]
    [(swap [t term:surface?])]
    [(parens [t term:surface?])]
    [(braces [t term:surface?])]
    [(brackets [t term:surface?])]
    [(text [t (listof term:surface?)])]
    [(text-form [cmd (or/c #f term:surface?)]
                [datums (or/c #f term:surface?)]
                [body (or/c #f term:surface?)])]
    [(file [lang term:surface:str?]
           [content term:surface?])]
    
    [(str [s string?])]    
    [(id [s symbol?])]
    ;; xxx be more specific
    [(num [n number?])]]
   [ast]])

