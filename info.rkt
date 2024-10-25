#lang info
(define collection "shindig")
(define deps '("base"))
(define build-deps
  '("scribble-lib" "racket-doc"
                   "rackunit-lib"
                   "reprovide-lang-lib"
                   "racket-langserver"
                   "buid"
                   "html-printer"
                   "data"
                   "file-watchers"
                   "dirname"
                   "http-easy"
                   "html-parsing"
                   "pollen"
                   "db"
                   "sqlite-table"
                   "sha"
                   "uuid"))
(define scribblings '(("scribblings/shindig.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(jay))
(define license '(Apache-2.0 OR MIT))
