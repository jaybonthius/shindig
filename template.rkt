#lang racket

(require pollen/core
         pollen/pagetree
         pollen/template
         pollen/setup
         sugar
         racket/pretty
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

(define (generate-toc str)
  (define top-level-pages (children 'pagetree-root))
  (case (current-poly-target)
    [(html)
     (if (or (not top-level-pages) (null? top-level-pages))
         ""
         `(nav [(class "table-of-contents")]
               (h2 "Shindig")
               (ul ,@(map make-toc-item top-level-pages))))]
    [(tex pdf)
     (define pagetree (get-pagetree (build-path (config:pollen-dir) "wholebook.ptree")))
     (define (wrap-include filename)
       (format "\\include{~a}" (remove-ext filename)))
     (define all-pages (pagetree->list pagetree))
     (string-join (map wrap-include all-pages) "\n")]))
