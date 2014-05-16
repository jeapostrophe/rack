#lang racket/base
(provide STDLIB)

(define STDLIB
  (hash
   '(number)
   (vector '(#%return 1)
           1)

   '(symbol)
   (vector '(#%return 'test)
           'test)

   '(string)
   (vector '(#%return "string")
           "string")

   '(null)
   (vector '(#%return #%null)
           '())

   '(cons)
   (vector '(#%return (#%cons 0 1))
           '(0 . 1))

   '(car)
   (vector '(#%return (#%car (#%cons 0 1)))
           0)

   '(cdr)
   (vector '(#%return (#%cdr (#%cons 0 1)))
           1)

   '(if#t)
   (vector '(#%return (#%if #t 0 1))
           0)

   '(if#f)
   (vector '(#%return (#%if #f 0 1))
           1)

   '(eq?#f)
   (vector '(#%return (#%eq? 1 0))
           #f)

   '(app)
   (vector '(#%return (#%app (#%lambda x (#%local x)) 1))
           1)

   '(link)
   (vector '(#%link (app) (#%return (#%top (app))))
           1)

   '(fun)
   (vector '(#%return (#%lambda x (#%cons 0 (#%local x))))
           'CLOSURE?)

   '(fun-call)
   (vector '(#%link (fun)
                    (#%return (#%app (#%lambda c (#%cons (#%car (#%local c))
                                                         (#%cdr (#%local c))))
                                     (#%app (#%top (fun)) 1))))
           '(0 . 1))

   '(mc-macro)
   (vector '(#%return (#%lambda stx (#%cons '#%return (#%cons 0 #%null))))
           'CLOSURE?)

   '(mc-transform)
   (vector '(#%transform (mc-macro)
                         (#%invoke (mc-macro) random stuff after))
           0)

   '(e-macro)
   (vector '(#%return (#%lambda stx 0))
           'CLOSURE?)

   '(e-transform)
   (vector '(#%transform (e-macro)
                         (#%return (#%invoke (e-macro) random stuff after)))
           0)

   '(e-macro-for-syntax)
   (vector '(#%link (number) (#%return (#%lambda stx (#%top (number)))))
           'CLOSURE?)

   '(e-transform-for-syntax)
   (vector '(#%transform (e-macro-for-syntax)
                         (#%return (#%invoke (e-macro-for-syntax) random stuff after)))
           1)

   '(e-macro-for-template)
   (vector '(#%template
             (number)
             (#%return
              (#%lambda stx
                        (#%cons '#%top (#%cons (#%cons 'number #%null) #%null)))))
           'CLOSURE?)

   '(e-transform-for-template)
   (vector '(#%transform (e-macro-for-template)
                         (#%return
                          (#%invoke (e-macro-for-template)
                                    random stuff after)))
           1)

   '(e-macro-for-template-for-syntax)
   (vector '(#%link
             (e-macro-for-template)
             (#%return
              (#%top (e-macro-for-template))))
           'CLOSURE?)

   '(e-transform-for-template-for-syntax)
   (vector '(#%transform (e-macro-for-template-for-syntax)
                         (#%return
                          (#%invoke (e-macro-for-template-for-syntax)
                                    random stuff after)))
           1)

   '(box)
   (vector '(#%return (#%box 0))
           'BOX?)
   '(unbox)
   (vector '(#%return (#%unbox (#%box 0)))
           '0)
   '(set-box!)
   (vector '(#%link (box) (#%return (#%set-box! (#%top (box)) 1)))
           '1)
   '(unbox2)
   (vector '(#%link (box) 
                    (#%link (set-box!)
                            (#%return (#%unbox (#%top (box))))))
           '1)

   ))
