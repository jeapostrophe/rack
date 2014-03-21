#lang rack/eu

(defn:fun:extern (type:atom:int 32) 'putchar (vector (vector 'c (type:atom:int 32))))

(defn:fun:local:ext (type:atom:int 32) 'main (vector)
  (stmt:let
   'A (expr:val:int 32 #x41)
   (stmt:let
    '_ (expr:call:ext (expr:global-ref 'putchar) (vector (expr:local-ref 'A)))
    (stmt:let
     '_ (expr:call:ext 
         (expr:global-ref 'putchar) 
         (vector 
          (expr:let 'B (expr:iadd (expr:local-ref 'A) (expr:val:int 32 1))
                    (expr:local-ref 'B))))
     (stmt:ret
      (expr:call:ext (expr:global-ref 'putchar)
                     (vector (expr:val:int 32 #xA))))))))
