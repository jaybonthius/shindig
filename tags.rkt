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
         racket/pretty
         racket/string
         sugar
         txexpr)

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
    [(tex pdf) `(txt "\\subsection*{" ,@text "}")]))

(define (h3 . text)
  (case (current-poly-target)
    [(html) `(h3 ,@text)]
    [(tex pdf) `(txt "\\subsubsection*{" ,@text "}")]))

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

(define (link url
              . text)
  (case (current-poly-target)
    [(tex pdf) `(txt-noescape ,(format "\\href{~a}{~a}" url (string-join text)))]
    [(html) `(a [(href ,url)] ,@text)]))

; Images
(define (image src alt)
  `(img ((src ,src) (alt ,alt))))

; Code
(define (inline-code . code)
  `(code ,@code))

(define (code . text)
  (case (current-poly-target)
    [(tex pdf) `(txt "\\texttt{" ,@text "}")]
    [(html) `(span [(class "code")] ,@text)]))

(define (code-block . text)
  (case (current-poly-target)
    [(tex pdf) `(txt "\\begin{verbatim}" ,@text "\\end{verbatim}")]
    [(html) `(pre [(class "code")] ,@text)]))

; Horizontal Rule
(define (horizontal-rule)
  `(hr))

(define (blockquote . words)
  (case (current-poly-target)
    [(pdf tex) `(txt "\\begin{quote}" ,@words "\\end{quote}")]
    [(html) `(blockquote ,@words)]))

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

(define (double-newline-replace lst)
  (define new-list (append (list "\n" "\n" "\n") lst))
  (define newline-counter 0)
  (for/list ([x (in-list new-list)])
    (if (string? x)
        (if (string=? x "\n")
            (@ (set! newline-counter (add1 newline-counter)) (if (> newline-counter 2) "\\item " x))
            (@ (set! newline-counter 0) x))
        (@ (set! newline-counter 0) x))))

(define (numbered-list . elems)
  (case (current-poly-target)
    [(tex pdf)
     (define new-elems (double-newline-replace elems))
     (@ `(txt "\\begin{enumerate}") `(txt ,@new-elems) `(txt "\\end{enumerate}"))]
    [(html)
     (define list-tag-function (make-list-function 'ol))
     (define elem-list (apply list-tag-function elems))
     (txexpr 'ol (get-attrs elem-list) (get-elements elem-list))]))

(define (bullet-list . elems)
  (case (current-poly-target)
    [(tex pdf)
     (define new-elems (double-newline-replace elems))
     (@ `(txt "\\begin{itemize}") `(txt ,@new-elems) `(txt "\\end{itemize}"))]
    [(html)
     (define list-tag-function (make-list-function 'ul))
     (define elem-list (apply list-tag-function elems))
     (list* (get-tag elem-list) (get-attrs elem-list) (get-elements elem-list))]))

(define (quick-table . tx-elements)
  (define text-rows (filter-not whitespace? tx-elements))
  (define rows-of-text-cells
    (for/list ([text-row (in-list text-rows)])
      (for/list ([text-cell (in-list (string-split text-row "|"))])
        (string-trim text-cell))))
  (pretty-print rows-of-text-cells)
  (case (current-poly-target)
    [(html)
     (match-define (list tr-tag td-tag th-tag) (map default-tag-function '(tr td th)))

     (define html-rows
       (match-let ([(cons header-row other-rows) rows-of-text-cells])
         (cons (map th-tag header-row)
               (for/list ([row (in-list other-rows)])
                 (map td-tag row)))))

     (cons 'table
           (for/list ([html-row (in-list html-rows)])
             (apply tr-tag html-row)))]
    [(tex pdf)
     (define col-count (length (first rows-of-text-cells)))
     (define col-format (make-string col-count #\c)) ; creates "ccc" for 3 columns
     (define rows
       (string-join (for/list ([row rows-of-text-cells])
                      (format "~a \\\\" (string-join row " & ")))
                    "\n"))
     `(txt-noescape ,(format "\\begin{tabular}{~a}" col-format) ,rows "\\end{tabular}")]))
