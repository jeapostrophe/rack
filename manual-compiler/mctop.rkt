#lang racket/base
(require racket/match)

(define (rrepl initial-st send unsend render)
  (let loop ([st initial-st])
    (match (read)
      [(? eof-object?) (exit 0)]
      [`(send ,str)
       (define-values (res nst) (send st str))
       (println `(response ,res ,(render nst)))
       (loop nst)]
      [`(unsend "")
       (define-values (res nst) (unsend st))
       (println `(response ,res ,(render nst)))
       (loop nst)]
      [_
       (println `(response "NO" ,(render st)))
       (loop st)])))

(module+ main
  (require racket/pretty
           racket/port
           racket/string)
  (printf "mctop!\n")

  (define (read-from-string s)
    (with-input-from-string s read))
  
  (define (echo-log)
    (rrepl null
           (λ (st str)
             (values "OK" (cons (read-from-string str) st)))
           (λ (st)
             (values "OK" (if (null? st) st (cdr st))))
           (λ (st)
             (pretty-format (reverse st)))))

  (rrepl #f
         (λ (st str)
           (match (read-from-string str)
             [`(compile ,prog)
              (define prog-str (string-join prog "\n"))
              (with-output-to-file "t.llvm"
                #:exists 'replace
                (λ () (display prog-str)))
              (define out prog-str)
              (values out st)]
             [_
              (values "NO" st)]))
         (λ (st)
           (values "NO" st))
         (λ (st)
           "")))
