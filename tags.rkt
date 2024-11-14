#lang racket/base

(require pollen/core
         pollen/decode
         pollen/render
         pollen/setup
         pollen/tag
         racket/file
         racket/list
         racket/match
         racket/path
         racket/port
         racket/string
         sugar)

(provide (all-defined-out))

(define-syntax-rule (define-heading heading-name tag)
  (define heading-name (default-tag-function tag #:class (symbol->string 'heading-name))))

(require racket/runtime-path)
(define-runtime-path template.EXT "template.EXT")

(define-heading chapter 'h1)
(define-heading section 'h2)

(define (h1 . text)
  (case (current-poly-target)
    [(html) `(h1 ,@text)]
    [(tex pdf) `(txt ,@text)]))

(define (h2 . text)
  (case (current-poly-target)
    [(html) `(h2 ,@text)]
    [(tex pdf) `(txt ,@text)]))

; Basic text formatting
(define (strong . text)
  (case (current-poly-target)
    [(html htm) `(strong ,@text)]
    [(tex pdf) `(txt "\\textbf{" ,@text "}")]))

(define (emph . text)
  (case (current-poly-target)
    [(html htm) `(em ,@text)]
    [(tex pdf) `(txt "\\emph{" ,@text "}")]))

(define (strike . text)
  `(del ,@text))

; Links
(define (link url
              . text)
  `(a ((href ,url)) ,@text))

; Images
(define (image src alt)
  `(img ((src ,src) (alt ,alt))))

; Code
(define (inline-code . code)
  `(code ,@code))

(define (code-block . code)
  `(pre (code ,@code)))

; Horizontal Rule
(define (horizontal-rule)
  `(hr))

(define (detect-list-items elems)
  (define elems-merged (merge-newlines elems))
  (define (list-item-break? elem)
    (define list-item-separator-pattern (regexp "\n\n\n+"))
    (and (string? elem) (regexp-match list-item-separator-pattern elem)))
  (define list-of-li-elems (filter-split elems-merged list-item-break?))
  (define list-of-li-paragraphs (map (λ (li) (decode-paragraphs li #:force? #t)) list-of-li-elems))
  (define li-tag (default-tag-function 'li))
  (map (λ (lip) (apply li-tag lip)) list-of-li-paragraphs))

(define ((make-list-function tag [attrs empty]) . args)
  (list* tag attrs (detect-list-items args)))

(define bullet-list (make-list-function 'ul))
(define numbered-list (make-list-function 'ol))

(define (quick-table . tx-elements)
  (define text-rows (filter-not whitespace? tx-elements))
  (define rows-of-text-cells
    (for/list ([text-row (in-list text-rows)])
      (for/list ([text-cell (in-list (string-split text-row "|"))])
        (string-trim text-cell))))

  (match-define (list tr-tag td-tag th-tag) (map default-tag-function '(tr td th)))

  (define html-rows
    (match-let ([(cons header-row other-rows) rows-of-text-cells])
      (cons (map th-tag header-row)
            (for/list ([row (in-list other-rows)])
              (map td-tag row)))))

  (cons 'table
        (for/list ([html-row (in-list html-rows)])
          (apply tr-tag html-row))))
