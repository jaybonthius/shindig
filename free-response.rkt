#lang racket/base

(require "utils.rkt"
         "sqlite.rkt")

(provide (all-defined-out))

(define question-id-param (make-parameter ""))

(define (free-response #:uid (uid "") . content)
  (validate-uid uid)
  (define question-id (string-append "fr-question-" uid))
  (define buttion-id (string-append "fr-button-" uid))

  (define evaluated-content
    (parameterize ([question-id-param uid])
      (map (lambda (x)
             (if (procedure? x)
                 (x)
                 x))
           content)))

  (define question-content
    `(div (div ,@evaluated-content) (button [(class "btn") (id ,buttion-id)] "Submit!")))

  ; TODO: uncomment this to enable HTMX loading
  (render-knowl (quote-xexpr-attributes question-content) 'free-response uid)
  `(div ((hx-get ,(format "/get-free-response/~a" uid)) (hx-trigger "load") (hx-target "this"))
        "Loading...")
  ; question-content
  )

(define ((fr-field #:uid [uid ""]
                   #:answer [answer ""]
                   #:placeholders [placeholders (hash)]
                   . content))
  (validate-uid uid)
  (define field-uid (string-append "fr-field-" uid))
  (define field-style-uid (string-append "fr-style-" uid))
  (define field-alerts-id (string-append "fr-alerts-" uid))
  (define buttion-id (string-append "fr-button-" (question-id-param)))

  (upsert-free-response uid (question-id-param) answer)

  `(div [(hx-get ,(format "/free-response/~a" uid))
         (hx-trigger "load")
         (hx-swap "none")
         (hx-select-oob ,(format "#~a:textContent,#~a:textContent" field-uid field-style-uid))
         (hx-ext "debug")]
        (math-field
         [(id ,field-uid)
          (name ,field-uid)
          (hx-post ,(format "/free-response/~a" uid))
          (hx-trigger ,(format "click from:#~a" buttion-id))
          ; TODO: do these if web-server is disabled
          ;  (hx-target ,(format "#~a" field-style-uid))
          ;  (hx-swap "innerHTML")
          (hx-select-oob ,(format "#~a:textContent,#~a:innerHTML" field-style-uid field-alerts-id))
          (style "display: block")])
        (style [(id ,field-style-uid)])
        (div [(id ,field-alerts-id)])))
