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
           racket/system
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
              (with-output-to-file "t.ll"
                #:exists 'replace
                (λ () (display prog-str)))
              (define the-string-port (open-output-string))
              (parameterize ([current-output-port the-string-port]
                             [current-error-port the-string-port])
                (printf "\nAssembling\n")
                (system* (find-executable-path "llvm-as") "t.ll" "-o" "t.bc")
                (printf "\nInspecting bitcode\n")
                (system* (find-executable-path "llvm-nm") "t.bc")
                (printf "\nInterpreting bitcode\n")
                (system* (find-executable-path "lli") "-O3" "t.bc")
                (printf "\nCompiling bitcode\n")
                (system* (find-executable-path "llc") "-O3"
                         "-filetype=obj" "t.bc" "-o" "t.o")
                (printf "\nInspecting object\n")
                (system* (find-executable-path "nm") "t.o")
                (printf "\nLinking object\n")
                (system* (find-executable-path "clang") "t.o" "-o" "t.exe")
                (printf "\nRunning binary\n")
                (system* "t.exe"))
              (define out (get-output-string the-string-port))
              (values out st)]
             [_
              (values "NO" st)]))
         (λ (st)
           (values "NO" st))
         (λ (st)
           "")))
