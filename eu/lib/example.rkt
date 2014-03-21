#lang rack/eu

(defn:fun:ext (type:atom:int:32) 'putchar (vector (vector 'c (type:atom:int:32))))

(defn:fun:int (type:atom:int:32) 'main (vector)
  (stmt:let
   '_
   (expr:call:ext (expr:global-ref 'putchar)
                  (vector (expr:val:int:32 #x41)))
   (stmt:ret
    (expr:call:ext (expr:global-ref 'putchar)
                   (vector (expr:val:int:32 #xA))))))
