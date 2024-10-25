#lang racket/base

(require pollen/tag
         pollen/core
         pollen/render
         pollen/decode
         sugar
         racket/file
         racket/path
         racket/port
         racket/list
         racket/string)

(provide (all-defined-out))

(define-syntax-rule (define-heading heading-name tag)
  (define heading-name (default-tag-function tag #:class (symbol->string 'heading-name))))

(require racket/runtime-path)
(define-runtime-path template.EXT "template.EXT")

(define-heading chapter 'h1)
(define-heading section 'h2)

; Basic text formatting
(define (strong . text)
  `(strong ,@text))

(define (emph . text)
  `(em ,@text))

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

(define (make-list-function tag [attrs empty])
  (λ args (list* tag attrs (detect-list-items args))))

(define bullet-list (make-list-function 'ul))
(define numbered-list (make-list-function 'ol))
