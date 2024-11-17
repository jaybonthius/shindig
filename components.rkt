#lang racket/base

(require pollen/core
         pollen/setup
         pollen/tag
         racket/path
         sugar
         "config.rkt"
         "utils.rkt")

(provide (all-defined-out))

(define ((make-component-function type) #:title title #:uid [uid ""] . body)
  (set! uid
        (if (string=? uid "")
            (to-kebab-case title)
            uid))
  (define id (format "~a-~a" (symbol->string type) uid))
  (define component
    `(div [(class ,(format "knowl ~a" type))]
          (strong ,(if (eq? type 'definition)
                       title
                       (apply string-append
                              (list (string-titlecase (symbol->string type)) ": " title))))
          (div ,@body)))

  (define abs-path (remove-ext* (hash-ref (current-metas) 'here-path)))
  (define source (find-relative-path (pollen-dir) abs-path))

  (upsert-xref type id title source)
  (case (current-poly-target)
    [(html)
     `(div ((id ,id) (class "knowl-container")
                     (component-type ,(symbol->string type))
                     (component-id ,uid))
           ,component)]
    [(tex pdf) "THIS SHOULD BE A REFERENCE TO A COMPONENT"]))

(define theorem (make-component-function 'theorem))
(define definition (make-component-function 'definition))
(define lemma (make-component-function 'lemma))


(define (pdf-download-button source-file)
  `(span [(class "downloads")]
         (a [(class "download pdf") (hx-boost "false") (href ,(pdf-name source-file))]
            "Download PDF")))
