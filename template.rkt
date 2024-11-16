#lang racket

(require pollen/core
         pollen/pagetree
         pollen/setup
         pollen/template
         racket/pretty
         sugar
         txexpr
         (prefix-in config: "config.rkt")
         "utils.rkt")

(provide (all-defined-out))

(define (make-page-link page-path)
  (printf (format "Making page link for ~a\n" page-path))
  `(a [(href ,(pollen-request (symbol->string page-path)))
       (hx-get ,(pollen-request (symbol->string page-path)))
       (hx-select "#main")
       (hx-swap "outerHTML")
       (hx-target "#main")
       (hx-push-url "true")
       (@click "activePage = '◊(baseurl)'")
       (:class "{ 'active': activePage === '/' }")]
      ,(select-from-metas 'title page-path)))

(define (make-toc-item page-path)
  (define child-pages (children page-path))
  (if (or (not child-pages) (null? child-pages))
      `(li ,(make-page-link page-path))
      `(li ,(make-page-link page-path) (ul ,@(map make-toc-item child-pages)))))

(define (generate-toc pagetree)
  (displayln (format "Generating TOC for ~a" pagetree))
  (case (current-poly-target)
    [(html)
     (define top-level-pages (children 'pagetree-root pagetree))
     (if (or (not top-level-pages) (null? top-level-pages))
         ""
         `(nav [(class "table-of-contents")]
               (h2 "Shindig")
               (ul ,@(map make-toc-item top-level-pages))))]
    [(tex pdf)
     (define top-level-pages (children 'pagetree-root pagetree))
     (displayln (format "Top-level pages: ~a" top-level-pages))

     (define frontmatter-pages
       (filter (λ (page) (string-prefix? (symbol->string page) "frontmatter/")) top-level-pages))

     ;  (define chapter-pages
     ;    (flatten (filter-map (λ (page)
     ;                           (and (string-prefix? (symbol->string page) "chapter/")
     ;                                (cons page (children page pagetree))))
     ;                         top-level-pages)))

     (define chapter-pages
       (filter (λ (page) (string-prefix? (symbol->string page) "chapter/")) top-level-pages))

     (define (wrap-include filename)
       (format "\\include{~a}" (remove-ext filename)))

     (string-join (list (wrap-include "title")
                        (string-join (map wrap-include frontmatter-pages) "\n")
                        (wrap-include "table-of-contents")
                        "\\mainmatter"
                        (string-join (map wrap-include chapter-pages) "\n"))
                  "\n")]))

(define (chapter-section-include chapter-pages)
  (define (wrap-include filename)
    (format "\\input{~a}" (remove-ext filename)))

  (string-join (list (string-join (map wrap-include chapter-pages) "\n")) "\n"))
