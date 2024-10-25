#lang racket/base

(require db
         json
         pollen/render
         pollen/setup
         racket/file
         racket/match
         racket/string
         (prefix-in config: "config.rkt")
         "sqlite.rkt")

(provide (all-defined-out))

; for utilities

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
                         (lambda ()
                           (displayln "◊(require html-printer)")
                           (displayln "◊(map xexpr->html5 (select-from-doc 'body here))"))))

  (render-to-file-if-needed temp-path template-path output-path)
  output-path
  "")

(define (upsert-question question-id answers)
  (define db-file (build-path (config:sqlite-path) "questions.sqlite"))
  (define conn (try-connect db-file))
  (when conn
    (with-handlers ([exn:fail? (lambda (e)
                                 (printf "Error during database operations: ~a\n" (exn-message e)))])
      ; todo: have this be a separate thing upon local setup
      (query-exec
       conn
       "CREATE TABLE IF NOT EXISTS questions (
                id TEXT PRIMARY KEY NOT NULL,
                answer JSON
            )")

      (define json-answers (jsexpr->string answers))

      (query-exec
       conn
       "INSERT OR REPLACE INTO questions (id, answer)
                   VALUES (?, json(?))"
       question-id
       json-answers)

      (disconnect conn)

      (printf "Database operations completed successfully.\n"))))

(define (upsert-free-response field-id question-id answer)
  (define db-file (build-path (config:sqlite-path) "free-response-questions.sqlite"))
  (define conn (try-connect db-file))
  (when conn
    (with-handlers ([exn:fail? (lambda (e)
                                 (printf "Error during database operations: ~a\n" (exn-message e)))])
      ; todo: have this be a separate thing upon local setup
      (query-exec
       conn
       "CREATE TABLE IF NOT EXISTS free_response_questions (
                field_id TEXT PRIMARY KEY NOT NULL,
                question_id TEXT NOT NULL,
                answer TEXT
            )")

      (query-exec
       conn
       "INSERT OR REPLACE INTO free_response_questions (field_id, question_id, answer)
                   VALUES (?, ?, ?)"
       field-id
       question-id
       answer)

      (disconnect conn)

      (printf "Database operations completed successfully.\n"))))

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

(define (to-kebab-case str)
  (string-join (map string-downcase (regexp-split #rx"[^a-zA-Z0-9]+" (string-trim str))) "-"))
