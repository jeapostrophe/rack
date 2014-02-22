#lang racket/base

(module+ main
  (for ([m (in-list '('#%kernel '#%unsafe '#%paramz '#%foreign '#%futures '#%network '#%place '#%expobs))])
    (define ns (make-base-empty-namespace))
    (define how-many 0)
    (define s
      (parameterize ([current-namespace ns])
        (namespace-require m)
        (namespace-mapped-symbols)))
    (printf "~a\n" m)
    (for ([s (in-list s)])
      (printf "\t~a\n" s))))
