#lang rack/eu

(defn:fun:extern (type:atom:int 32) 'putchar (vector (vector 'c (type:atom:int 32))))

(defn:fun:local:ext (type:atom:int 32) 'f (vector)
  (stmt:ret
   (expr:call:ext (expr:global-ref 'putchar)
                  (vector (expr:val:int 32 #x41)))))

(defn:fun:local:ext (type:atom:int 32) 'main (vector)
  (stmt:let
   'A (expr:convert:f2i:u
       (expr:float:mul
        (expr:convert:i2f:s
         (expr:int:mul
          (expr:convert:f2i:s
           (expr:convert:f2f:trunc
            (expr:convert:f2f:ext
             (expr:val:float 32 -35.0)
             (type:atom:float 64))
            (type:atom:float 32))
           (type:atom:int 32))
          (expr:convert:i2i:trunc
           (expr:convert:i2i:sext
            (expr:val:int 32 -1)
            (type:atom:int 64))
           (type:atom:int 32)))
         (type:atom:float 32))
        (expr:convert:i2f:u
         (expr:convert:i2i:trunc
          (expr:convert:i2i:zext
           (expr:val:int 32 2)
           (type:atom:int 64))
          (type:atom:int 32))
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
