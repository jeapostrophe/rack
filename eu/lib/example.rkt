#lang rack/eu

(defn:fun:extern (type:atom:int 32) 'putchar (vector (vector 'c (type:atom:int 32))))

(defn:fun:local:ext (type:atom:int 32) 'f (vector)
  (stmt:ret
   (expr:call:ext (expr:global-ref 'putchar)
                  (vector (expr:val:int 32 #x41)))))

(defn:fun:local:ext (type:atom:int 32) 'main (vector)
  (stmt:let
   'A (expr:f->iu
       (expr:float:mul
        (expr:is->f
         (expr:int:mul
          (expr:f->is
           (expr:val:float 32 -35.0)
           (type:atom:int 32))
          (expr:val:int 32 -1))
         (type:atom:float 32))
        (expr:iu->f
         (expr:val:int 32 2)
         (type:atom:float 32)))
       (type:atom:int 32))
   (stmt:let
    '_ (expr:call:ext (expr:global-ref 'putchar) (vector (expr:local-ref 'A)))
    (stmt:let
     '_ (expr:call:ext
         (expr:global-ref 'putchar)
         (vector
          (expr:let 'B (expr:int:add (expr:local-ref 'A) (expr:val:int 32 1))
                    (expr:local-ref 'B))))
     (stmt:ret
      (expr:call:ext (expr:global-ref 'putchar)
                     (vector (expr:val:int 32 #xA))))))))
