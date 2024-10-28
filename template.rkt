#lang racket

(require pollen/core
         pollen/pagetree
         pollen/template
         racket/pretty
         txexpr
         "utils.rkt")

(provide (all-defined-out))

(define (make-page-link page-path)
  (printf (format "Making page link for ~a\n" page-path))
  `(a [[href ,(pollen-request (symbol->string page-path))]] ,(select-from-metas 'title page-path)))

(define (make-toc-item page-path)
  (define child-pages (children page-path))
  (if (or (not child-pages) (null? child-pages))
      `(li ,(make-page-link page-path))
      `(li ,(make-page-link page-path) (ul ,@(map make-toc-item child-pages)))))

(define (generate-toc str)
  (define top-level-pages (children 'pagetree-root))
  (printf "Top-level pages: ~a\n" top-level-pages)
  (if (or (not top-level-pages) (null? top-level-pages))
      ""
      `(nav [(class "table-of-contents")] (h2 "Shindig") (ul ,@(map make-toc-item top-level-pages)))))
