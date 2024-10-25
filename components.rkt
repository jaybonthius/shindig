#lang racket/base

(require pollen/core
         pollen/tag
         racket/path
         sugar
         "utils.rkt"
         "config.rkt")

(provide (all-defined-out))

(define ((make-component-function type) #:title title #:uid [uid ""] . body)
  (set! uid
        (if (string=? uid "")
            (to-kebab-case title)
            uid))
  (define id (format "~a-~a" (symbol->string type) uid))
  (define component
    `(div [(class "knowl")]
          (strong ,(if (eq? type 'definition)
                       title
                       (apply string-append
                              (list (string-titlecase (symbol->string type)) ": " title))))
          (div ,@body)))

  (define source (remove-ext* (file-name-from-path (hash-ref (current-metas) 'here-path))))

  (upsert-xref type id title source)
  `(div ((id ,id) (class "knowl-container")
                  (component-type ,(symbol->string type))
                  (component-id ,uid))
        ,component))

(define theorem (make-component-function 'theorem))
(define definition (make-component-function 'definition))
(define lemma (make-component-function 'lemma))
