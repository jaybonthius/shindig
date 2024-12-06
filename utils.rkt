#lang racket/base

(require db
         json
         pollen/render
         pollen/setup
         racket/file
         racket/match
         racket/path
         racket/string
         sugar
         (prefix-in config: "config.rkt")
         "sqlite.rkt")

(provide (all-defined-out))

; for utilities

(define (pollen-request req)
  (string-append (config:baseurl)
                 (cond
                   [(config:pretty-url)
                    (if (has-ext? req "html")
                        (path->string (remove-ext req))
                        req)]
                   [(has-ext? req "html") req]
                   [else (path->string (add-ext req "html"))])))

(define (validate-uid uid)
  (when (or (not (string? uid)) (string=? uid ""))
    (raise-argument-error 'fr-field "non-empty string" uid)))

(define (quote-xexpr-attributes xexpr)
  (match xexpr
    [(list tag (and attrs (list (list _ _) ...)) content ...)
     (list* tag `'(,@attrs) (map quote-xexpr-attributes content))]
    [(list tag content ...) (cons tag (map quote-xexpr-attributes content))]
    [_ xexpr]))

(define (render-knowl xexpr type uid)
  (define output-dir (build-path (config:pollen-dir) "knowl" (symbol->string type)))
  (define temp-dir (build-path output-dir "temp"))
  (define temp-path (build-path temp-dir (string-append uid ".html.pm")))
  (for-each make-directory* (list temp-dir output-dir))

  (with-output-to-file temp-path
                       (lambda ()
                         (displayln "#lang pollen")
                         (display "◊")
                         (write xexpr))
                       #:exists 'replace)

  (define output-path (build-path output-dir (string-append uid ".html")))
  (define template-path (build-path output-dir "template.html.p"))
  (unless (file-exists? template-path)
    (with-output-to-file template-path
                         (lambda () (displayln "◊(map ->html (select-from-doc 'body here))"))))

  (render-to-file-if-needed temp-path template-path output-path)
  output-path
  "")

(define (upsert-xref type id title source)
  (define db-file (build-path (config:sqlite-path) "cross-references.sqlite"))

  (define conn (try-connect db-file))
  (when conn
    (with-handlers ([exn:fail? (lambda (e)
                                 (printf "Error during database operations: ~a\n" (exn-message e)))])

      (query-exec
       conn
       "INSERT OR REPLACE INTO cross_references (type, id, title, source)
                   VALUES (?, ?, ?, ?)"
       (symbol->string type)
       id
       title
       (path->string source))

      (disconnect conn)

      (printf "Database operations completed successfully.\n"))))

(define (upsert-tag id source entry subentry)
  (define db-file (build-path (config:sqlite-path) "book-index.sqlite"))

  (define conn (try-connect db-file))
  (when conn
    (with-handlers ([exn:fail? (lambda (e)
                                 (printf "Error during database operations: ~a\n" (exn-message e)))])

      (query-exec
       conn
       "INSERT OR REPLACE INTO book_index (id, source, entry, subentry)
                   VALUES (?, ?, ?, ?)"
       id
       (path->string source)
       entry
       subentry)

      (disconnect conn)

      (printf "Database operations completed successfully.\n"))))

(define (to-kebab-case str)
  (string-join (map string-downcase (regexp-split #rx"[^a-zA-Z0-9]+" (string-trim str))) "-"))

(define (string->boolean str)
  (cond
    [(boolean? str) str]
    [else
     (define trimmed (string-downcase (string-trim str)))
     (cond
       [(member trimmed '("#t" "#true" "true")) #t]
       [(member trimmed '("#f" "#false" "false")) #f]
       [else (error "Invalid boolean string:" str)])]))

; this is pretty indiscriminate, but it's fine for now
; TODO: make this more robust (check pdf pagetree for this file?)
(define (pdfable? file-path)
  (string-contains? file-path ".poly"))

(define (pdf-name page)
  (string-replace (path->string (file-name-from-path page)) "poly.pm" "pdf"))
