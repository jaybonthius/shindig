#lang racket/base

(require pollen/core
         pollen/setup
         pollen/tag
         racket/path
         racket/string
         sugar
         "config.rkt"
         "utils.rkt")

(provide (all-defined-out))

(define ((make-component-function type #:display [display #f]) #:title title #:uid [uid ""] . body)
  (set! uid
        (if (string=? uid "")
            (to-kebab-case title)
            uid))

  (define display-title
    (if display
        display
        (string-titlecase (symbol->string type))))

  (case (current-poly-target)
    [(html)
     (define id (format "~a-~a" (symbol->string type) uid))
     (define component
       `(div [(class ,(format "knowl ~a" type))]
             (strong ,(apply string-append (list display-title ": " title)))
             (div ,@body)))

     (define abs-path (remove-ext* (hash-ref (current-metas) 'here-path)))
     (define source (find-relative-path (pollen-dir) abs-path))

     (upsert-xref type id title source)

     `(div ((id ,id) (class "knowl-container")
                     (component-type ,(symbol->string type))
                     (component-id ,uid))
           ,component)]
    [(tex pdf)
     `(txt
       ,(format "\\begin{tcolorbox}[~a, title={~a: ~a}]" (symbol->string type) display-title title)
       ,(format "\\label{~a:~a}" (symbol->string type) uid)
       ,@body
       "\\end{tcolorbox}")]))

(define theorem (make-component-function 'theorem))
(define definition (make-component-function 'definition))
(define lemma (make-component-function 'lemma))
(define preview-activity (make-component-function 'preview-activity #:display "Preview Activity"))
(define activity (make-component-function 'activity))

(define (pdf-download-button source-file)
  `(span [(class "downloads")]
         (a [(class "download pdf") (hx-boost "false") (href ,(pdf-name source-file))]
            "Download PDF")))
