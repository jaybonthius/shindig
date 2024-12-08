#lang racket

(require db
         pollen/core
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
  (define request (pollen-request (symbol->string page-path)))
  `(a [(href ,request)
       (hx-get ,request)
       (hx-select "#main")
       (hx-swap "outerHTML")
       (hx-target "#main")
       (hx-push-url "true")
       (@click "activePage = '◊(baseurl)'")
       (:class "{ 'active': activePage === '/' }")]
      ,(select-from-metas 'title page-path)))

(define (make-toc-item page-path)
  (define child-pages (children page-path))
  (cond
    [(or (not child-pages) (null? child-pages))
     (if (has-ext? page-path "html")
         `(li ,(make-page-link page-path))
         '())]
    [(has-ext? page-path "html")
     `(li ,(make-page-link page-path) (ul ,@(map make-toc-item child-pages)))]
    [else `(li ,@(map make-toc-item child-pages))])) ; Just render children for non-pages

(define (generate-toc pagetree)
  (case (current-poly-target)
    [(html)
     (define top-level-pages 
       (filter (λ (page) 
                (not (equal? page 'index.html)))
              (children 'pagetree-root pagetree)))
     (displayln (format "Top-level pages: ~a" top-level-pages))
     (if (or (not top-level-pages) (null? top-level-pages))
         ""
         `(nav [(class "table-of-contents")]
               (a [(href "/")] (h2 ,(config:book-title)))
               (ul ,@(map make-toc-item top-level-pages))))]
    [(tex pdf)
     (define top-level-pages (children 'pagetree-root pagetree))
     (define all-pages (pagetree->list pagetree))

     (define frontmatter-pages
       (filter (λ (page) (string-prefix? (symbol->string page) "frontmatter/")) all-pages))

     (define chapter-pages
       (flatten (filter-map (λ (page)
                              (and (string-prefix? (symbol->string page) "chapter/")
                                   (cons page (children page pagetree))))
                            top-level-pages)))

     (define (wrap-include filename)
       (format "\\include{~a}" (remove-ext filename)))

     (string-join (list "\\frontmatter"
                        (string-join (map wrap-include frontmatter-pages) "\n")
                        "\\mainmatter"
                        (string-join (map wrap-include chapter-pages) "\n"))
                  "\n")]))

(define (generate-input-toc pagetree)
  (case (current-poly-target)
    [(html)
     (define top-level-pages (children 'pagetree-root pagetree))
     (if (or (not top-level-pages) (null? top-level-pages))
         ""
         `(nav [(class "table-of-contents")]
               (a [(href "/")] (h2 ,(config:book-title)))
               (ul ,@(map make-toc-item top-level-pages))))]
    [(tex pdf)
     (define top-level-pages (children 'pagetree-root pagetree))
     (define all-pages (pagetree->list pagetree))

     (define frontmatter-pages
       (filter (λ (page) (string-prefix? (symbol->string page) "frontmatter/")) all-pages))

     (define backmatter-pages
       (filter (λ (page) (string-prefix? (symbol->string page) "backmatter/")) all-pages))

     (define chapter-pages
       (flatten (filter-map (λ (page)
                              (and (string-prefix? (symbol->string page) "chapter/")
                                   (cons page (children page pagetree))))
                            top-level-pages)))

     (define (wrap-input filename)
       (format "\\input{~a}" (remove-ext filename)))

     (string-join (list "\\maketitle"
                        "\\frontmatter"
                        (string-join (map wrap-input frontmatter-pages) "\n")
                        "\\tableofcontents"
                        "\\mainmatter"
                        (string-join (map wrap-input chapter-pages) "\n")
                        "\\backmatter"
                        (string-join (map wrap-input backmatter-pages) "\n"))
                  "\n")]))

(define (get-all-index-tags)
  (define db-connection
    (sqlite3-connect #:database (build-path (config:sqlite-path) "book-index.sqlite")))
  (with-handlers ([exn:fail? (lambda (e)
                               (displayln (format "Database error: ~a" (exn-message e)))
                               #f)])
    (define result (query-rows db-connection "SELECT * FROM book_index"))
    (if (null? result)
        #f
        (for/list ([row (in-list result)])
          (make-hash (list (cons 'id (vector-ref row 0))
                           (cons 'source (vector-ref row 1))
                           (cons 'entry (vector-ref row 2))
                           (cons 'subentry (vector-ref row 3))))))))

(define (page-index-link source id)
  (define link (format "~a#~a" (pollen-request source) id))
  `(a [(href ,link)] ,link))

(define (generate-index)
  (case (current-poly-target)
    [(html)
     (define all-tags (get-all-index-tags))
     (case all-tags
       [(#f)
        (displayln "No tags found.")
        "Placeholder for a future index."]
       [else
        (define processed-tags
          (for/fold ([result '()]) ([group (group-by (lambda (h) (hash-ref h 'entry)) all-tags)])
            (define entry-name (hash-ref (car group) 'entry))
            (define sources
              (for/list ([member group]
                         #:when (equal? (hash-ref member 'subentry) ""))
                (hash 'source (hash-ref member 'source) 'id (hash-ref member 'id))))

            (define subentries
              (for/fold ([result '()])
                        ([group (filter (lambda (g) (not (equal? (hash-ref (car g) 'subentry) "")))
                                        (group-by (lambda (h) (hash-ref h 'subentry)) group))])
                (define subentry-name (hash-ref (car group) 'subentry))

                (define sources
                  (for/list ([member group])
                    (hash 'source (hash-ref member 'source) 'id (hash-ref member 'id))))

                (cons (hash 'subentry subentry-name 'sources sources) result)))
            (cons (hash 'entry entry-name 'sources sources 'subentries subentries) result)))

        `(div [(class "index")]
              (h2 "Index")
              (ul ,@(for/list ([entry processed-tags])
                      `(li ,(hash-ref entry 'entry)
                           (@ (ul ,@(for/list ([source (hash-ref entry 'sources)])
                                      `(li ,(page-index-link (hash-ref source 'source)
                                                             (hash-ref source 'id)))))
                              ,(unless (null? (hash-ref entry 'subentries))
                                 `(ul ,@(for/list ([source (hash-ref entry 'subentries)])
                                          (@ `(li ,(hash-ref source 'subentry))
                                             `(ul ,@(for/list ([source (hash-ref source 'sources)])
                                                      `(li ,(page-index-link
                                                             (hash-ref source 'source)
                                                             (hash-ref source 'id))))))))))))))])]
    [(tex pdf) "\\printindex"]))
