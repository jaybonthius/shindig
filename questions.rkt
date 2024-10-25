#lang racket/base

(require db
         json
         pollen/decode
         pollen/render
         pollen/template
         racket/file
         racket/list
         racket/pretty
         racket/set
         racket/string
         xml
         "utils.rkt"
         "sqlite.rkt")

(provide (all-defined-out))

(require racket/match)

(define (option #:correct [correct #f] #:id [id ""] . content)
  `(option ((correct ,correct) (id ,id)) ,@content))

(define (get-correct-answers items)
  (define (process-item item)
    (cond
      [(and (list? item) (eq? (car item) 'option))
       (define attrs (cadr item))
       (if (and (list? attrs) (assq 'correct attrs) (eq? #t (cadr (assq 'correct attrs))))
           (list (cadr (assq 'id attrs)))
           '())]
      [(list? item) (process-items item)]
      [else '()]))

  (define (process-items items)
    (apply append (map process-item items)))
  (process-items items))

(define (new-question #:uuid [uuid #f] #:multichoice [multichoice #f] . content)
  (define option-type (if multichoice "checkbox" "radio"))

  (define (render-option item)
    (cond
      [(and (list? item) (eq? (car item) 'option))
       (define attrs (cadr item))
       (define id (string-join (list uuid (cadr (assq 'id attrs))) "-"))
       (define option-content (cddr item))
       (define correct-option "btn-success")
       `(div
         ((class "flex items-center"))
         (label
          ((for ,id
             )
           (class ,(format
                    "btn btn-block justify-start items-center ●(if (not (equal? selected-answers \"none\")) (if (set-member? selected-answers \"~a\") (if (set-member? correct-answers \"~a\") \"btn-success\" \"btn-error\" ) \"btn-ghost\" ) \"\" )"
                    id
                    id)))
          (input ((type ,option-type) (id ,id)
                                      (name ,uuid)
                                      (value ,id)
                                      (class ,(string-join (list option-type "mr-4") " "))))
          ,@option-content))]
      [else item]))

  (define (split-content items)
    (let loop ([items items]
               [question-content '()]
               [options '()])
      (cond
        [(null? items) (values (reverse question-content) (reverse options))]
        [else
         (define item (car items))
         (if (and (list? item) (eq? (car item) 'option))
             (loop (cdr items) question-content (cons item options))
             (loop (cdr items) (cons item question-content) options))])))

  (define (remove-trailing-newlines items)
    (let loop ([items (reverse items)]
               [result '()])
      (cond
        [(null? items) result]
        [(and (string? (car items)) (string=? (car items) "\n"))
         (if (null? result)
             (loop (cdr items) result)
             (reverse items))]
        [else (reverse items)])))

  (define-values (question-content options) (split-content content))
  (define cleaned-question-content (remove-trailing-newlines question-content))
  (define rendered-options `(div ((class "space-y-2")) ,@(map render-option options)))
  (define submit-button `(button ((type "submit") (class "btn")) "Submit!"))

  (define correct-answers
    (map (lambda (answer) (string-append uuid "-" answer)) (get-correct-answers content)))

  (define question-content-id (string-append "question-content-" uuid))

  (define expression
    `(form ((id ,uuid) (hx-post ,(format "/check_answers/~a" uuid))
                       (hx-target "this")
                       (hx-include "this")
                       (hx-swap "outerHTML"))
           ,@cleaned-question-content
           ,rendered-options
           ,submit-button))

  (define question-getter
    `(div ((id ,question-content-id) (hx-get ,(format "/question_detail/~a" uuid))
                                     (hx-trigger "load")
                                     (hx-target ,(string-append "#" question-content-id)))
          "Loading..."))

  (upsert-question uuid correct-answers)
  (render-knowl (quote-xexpr-attributes expression) 'question uuid)

  question-getter)

(define (free-response-question #:uuid [uuid #f] answer)
  (upsert-question uuid answer)
  `(div (div ((class "not-prose my-2")) (math-field [(style "width: 30%") (id ,uuid)] ""))
        (div [(id "css-container")] "")
        (div (button [(class "btn")
                      (hx-post "/check-free-response")
                      (hx-vals ,(format "js:{latex: document.getElementById('~a').value, uuid: '~a'}"
                                        uuid
                                        uuid))
                      (hx-target "#css-container")
                      (hx-swap "innerHTML")]
                     "Submit!"))))
