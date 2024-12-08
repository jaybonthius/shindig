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
         rackunit
         simple-qr
         sugar
         txexpr
         uuid
         (prefix-in config: "config.rkt")
         (prefix-in utils: "utils.rkt"))

(provide (all-defined-out))

(define-syntax-rule (define-heading heading-name tag)
  (define heading-name (default-tag-function tag #:class (symbol->string 'heading-name))))

(require racket/runtime-path)
(define-runtime-path template.EXT "template.EXT")

; (define-heading subsection 'h2)

(define (h1 . text)
  (case (current-poly-target)
    [(html) `(h1 ,@text)]
    [(tex pdf) `(txt ,@text)]))

(define (subsection . text)
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
    [(tex pdf)
     `(txt-noescape ,(format "\\href{~a}{~a}"
                             url
                             (if (null? text)
                                 url
                                 (string-join text))))]
    [(html)
     `(a [(href ,url)]
         ,@(if (null? text)
               (list url)
               text))]))

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

(define (motivating-questions . elems)
  (case (current-poly-target)
    [(tex pdf)
     (define new-elems (double-newline-replace elems))
     `(txt "\\textbf{Motivating Questions} \\begin{itemize}" ,@new-elems "\\end{itemize}")]
    [(html)
     (define list-tag-function (make-list-function 'ul))
     (define elem-list (apply list-tag-function elems))
     `(div [(class "motivating-questions")]
           (h3 "Motivating Questions")
           ,(list* (get-tag elem-list) (get-attrs elem-list) (get-elements elem-list)))]))

(define (quick-table . tx-elements)
  (define text-rows (filter-not whitespace? tx-elements))
  (define rows-of-text-cells
    (for/list ([text-row (in-list text-rows)])
      (for/list ([text-cell (in-list (string-split text-row "|"))])
        (string-trim text-cell))))
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

(define (sidenote . text)
  (case (current-poly-target)
    [(html)
     `(@ (button
          [(type "button") (class "sidenote-button") (_ "on click toggle .show on next .sidenote")]
          (i [(class "fa-solid fa-plus")]))
         (span [(class "sidenote")] (span ,@text)))]
    [(tex pdf) `(txt "\\sidenote{" ,@text "}")]))

(define (geogebra source #:fullwidth (fullwidth #t) #:aspect-ratio (aspect-ratio "16/9") text)
  (define url
    (format
     "https://www.geogebra.org/material/iframe/id/~a/width/~a/height/~a/border/ffffff/sfsb/false/smb/false/stb/false/stbh/false/ai/false/asb/false/sri/true/rc/false/ld/false/sdz/false/ctl/false"
     source
     (if fullwidth "960" "720")
     "540"))
  (case (current-poly-target)
    [(html)
     `(iframe ((scrolling "no") (src ,url)
                                (class ,(string-append "geogebra" (if fullwidth " fullwidth" "")))
                                (loading "lazy")
                                (style ,(format "aspect-ratio: ~a" aspect-ratio))
                                (frameBorder "0")))]
    [(tex pdf)
     (define qr-code-name (format "geogebra-~a.png" source))
     (define qr-code-path (build-path (config:qr-codes-path) qr-code-name))
     (define url (format "https://www.geogebra.org/classic/~a" source))
     (qr-write url (path->string qr-code-path))
     `(txt ,(format "\\includegraphics[width=0.5\\textwidth]{~a}" (path->string qr-code-path)))

     `(txt ,(string-append "\\begin{minipage}{0.2\\textwidth}\n"
                           "  \\includegraphics[width=\\textwidth]{"
                           (path->string qr-code-path)
                           "}\n"
                           "\\end{minipage}\\hfill"
                           "\\begin{minipage}{0.75\\textwidth}\n"
                           "  \\vspace{1em}\\url{"
                           url
                           "}\n"
                           "\\end{minipage}"))]))

(define (iframe url
                #:fullwidth (fullwidth #t)
                #:aspect-ratio (aspect-ratio "16/9")
                #:scrolling (scrolling #t))
  (case (current-poly-target)
    [(html)
     `(iframe ((scrolling ,(if scrolling "yes" "no"))
               (src ,url)
               (class ,(string-append "iframe" (if fullwidth " fullwidth" "")))
               (loading "lazy")
               (style ,(format "aspect-ratio: ~a" aspect-ratio))
               (frameBorder "0")))]
    [(tex pdf)
     (define url-hash (equal-hash-code url))
     (define qr-code-name (format "iframe-~a.png" url-hash))
     (define qr-code-path (build-path (config:qr-codes-path) qr-code-name))
     (qr-write url (path->string qr-code-path))
     `(txt ,(string-append "\\begin{minipage}{0.2\\textwidth}\n"
                           "  \\includegraphics[width=\\textwidth]{"
                           (path->string qr-code-path)
                           "}\n"
                           "\\end{minipage}\\hfill"
                           "\\begin{minipage}{0.75\\textwidth}\n"
                           "  \\vspace{1em}\\url{"
                           url
                           "}\n"
                           "\\end{minipage}"))]))

(define (sage-cell #:evaluate-button-text (button-text "Evaluate")
                   #:template (template "minimal")
                   . code)
  (case (current-poly-target)
    [(html) `(div ((class "compute")) (script ((type "text/x-sage")) ,@code))]
    [(tex pdf) `(txt "\\begin{pythoncode}\n" ,@code "\n\\end{pythoncode}")]))

(define (caption . text)
  (case (current-poly-target)
    [(tex pdf) `(txt "\\marginnote{" ,@text "}")]
    [(html) `(span [(class "marginnote caption")] ,@text)]))

(define (tag #:entry [entry ""] #:subentry [subentry ""] . text)
  (when (and (non-empty-string? subentry) (not (non-empty-string? entry)))
    (error 'tag "entry must be provided when subentry is provided"))

  (unless (non-empty-string? entry)
    (set! entry (apply string-append text)))

  (case (current-poly-target)
    [(tex pdf)
     (define latex-tag
       (if (non-empty-string? subentry)
           (string-append entry "!" subentry)
           entry))
     `(txt ,@text "\\index{" ,latex-tag "}")]
    [(html)
     (define abs-path (remove-ext* (hash-ref (current-metas) 'here-path)))
     (define source (find-relative-path (config:pollen-dir) abs-path))
     (define html-id (format "tag-~a" (equal-hash-code (string-join (list entry subentry) " "))))
     (utils:upsert-tag html-id source entry subentry)
     `(span ((class "tag") [id ,html-id]) ,@text)]))

(define (image source #:fullwidth [fullwidth #f] #:width [width 1.0] . text)
  (define server-image-path (format "~astatic/media/images/~a" (config:baseurl) source))
  (define local-image-path (build-path (config:images-path) source))
  (define fig-class (if fullwidth "fullwidth" "halfwidth"))
  (define width-str (format "~a\\textwidth" width))
  (case (current-poly-target)
    [(tex pdf)
     `(txt "\\begin{center}"
           ,(format "\\includegraphics[width=~a]" width-str)
           ,(format "{~a}" local-image-path)
           ,@(if (eq? empty text)
                 '()
                 `("\\captionof{figure}{" ,@text "}"))
           "\\end{center}")]
    [(html)
     `(figure [(class ,fig-class)]
              (img ([src ,server-image-path] [style ,(format "width: ~a%" (* width 100))]))
              ,(if (eq? empty text)
                   ""
                   (apply caption text)))]))

(define (youtube-iframe url
                        #:fullwidth (fullwidth #t)
                        #:aspect-ratio (aspect-ratio "16/9")
                        #:controls (controls #t))

  (define video-id (extract-youtube-id url))

  (case (current-poly-target)
    [(html)
     `(iframe
       ((src ,(format "https://www.youtube.com/embed/~a~a" video-id (if controls "" "?controls=0")))
        (class ,(string-append "youtube-iframe" (if fullwidth " fullwidth" "")))
        (loading "lazy")
        (style ,(format "aspect-ratio: ~a" aspect-ratio))
        (frameBorder "0")
        (allow
         "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture")
        (allowFullScreen "")))]
    [(tex pdf)
     (define full-url (format "https://youtu.be/~a" video-id))
     (define url-hash (equal-hash-code full-url))
     (define qr-code-name (format "youtube-~a.png" url-hash))
     (define qr-code-path (build-path (config:qr-codes-path) qr-code-name))
     (qr-write full-url (path->string qr-code-path))
     `(txt ,(string-append "\\begin{minipage}{0.2\\textwidth}\n"
                           "  \\includegraphics[width=\\textwidth]{"
                           (path->string qr-code-path)
                           "}\n"
                           "\\end{minipage}\\hfill"
                           "\\begin{minipage}{0.75\\textwidth}\n"
                           "  \\vspace{1em}\\url{"
                           full-url
                           "}\n"
                           "\\end{minipage}"))]))

(define (extract-youtube-id url)
  ; First try to match standard youtube.com URLs
  (define watch-pattern #px"[?&]v=([a-zA-Z0-9_-]{11})")
  (define shorts-pattern #px"/shorts/([a-zA-Z0-9_-]{11})")
  (define embed-pattern #px"/embed/([a-zA-Z0-9_-]{11})")
  (define short-pattern #px"youtu\\.be/([a-zA-Z0-9_-]{11})")
  (define direct-pattern #px"^[a-zA-Z0-9_-]{11}$")

  (define (extract-match pattern)
    (define matches (regexp-match pattern url))
    (and matches (let ([id (cadr matches)]) (and (regexp-match? direct-pattern id) id))))

  (or (extract-match watch-pattern)
      (extract-match shorts-pattern)
      (extract-match embed-pattern)
      (extract-match short-pattern)
      (and (regexp-match? direct-pattern url) url)
      (error 'extract-youtube-id "Invalid YouTube URL or video ID: ~a" url)))

; Unit Tests
(module+ test
  (test-case "Standard YouTube URLs"
    (check-equal? (extract-youtube-id "https://www.youtube.com/watch?v=dQw4w9WgXcQ") "dQw4w9WgXcQ")
    (check-equal? (extract-youtube-id "https://youtube.com/watch?v=dQw4w9WgXcQ") "dQw4w9WgXcQ")
    (check-equal? (extract-youtube-id "https://www.youtube.com/watch?v=dQw4w9WgXcQ&feature=share")
                  "dQw4w9WgXcQ"))

  (test-case "Short YouTube URLs"
    (check-equal? (extract-youtube-id "https://youtu.be/dQw4w9WgXcQ") "dQw4w9WgXcQ")
    (check-equal? (extract-youtube-id "https://youtu.be/dQw4w9WgXcQ?t=10") "dQw4w9WgXcQ")
    (check-equal? (extract-youtube-id "https://youtu.be/SXOHCiukZPw?si=VegHYqE-JDLm5nAy")
                  "SXOHCiukZPw"))

  (test-case "Embed URLs"
    (check-equal? (extract-youtube-id "https://www.youtube.com/embed/dQw4w9WgXcQ") "dQw4w9WgXcQ")
    (check-equal? (extract-youtube-id "<iframe src=\"https://www.youtube.com/embed/dQw4w9WgXcQ\">")
                  "dQw4w9WgXcQ"))

  (test-case "YouTube Shorts URLs"
    (check-equal? (extract-youtube-id "https://www.youtube.com/shorts/dQw4w9WgXcQ") "dQw4w9WgXcQ")
    (check-equal? (extract-youtube-id "https://youtube.com/shorts/dQw4w9WgXcQ?feature=share")
                  "dQw4w9WgXcQ"))

  (test-case "Direct video IDs"
    (check-equal? (extract-youtube-id "dQw4w9WgXcQ") "dQw4w9WgXcQ"))

  (test-case "Invalid URLs"
    (check-exn exn:fail? (λ () (extract-youtube-id "https://www.youtube.com/watch")))
    (check-exn exn:fail? (λ () (extract-youtube-id "https://youtu.be/")))
    (check-exn exn:fail? (λ () (extract-youtube-id "not-a-youtube-url")))
    (check-exn exn:fail? (λ () (extract-youtube-id "dQw4w9WgXc"))) ; Too short
    (check-exn exn:fail? (λ () (extract-youtube-id "dQw4w9WgXcQQ")))) ; Too long
  )
