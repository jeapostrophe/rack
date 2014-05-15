(module reader syntax/module-reader
  #:read ck-read
  #:read-syntax ck-read-syntax
  #:whole-body-readers? #t
  #:language 'rack/eu/lang
  (require rack/ck/parser))
