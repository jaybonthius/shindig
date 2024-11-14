#lang racket/base

(require pollen/decode
         pollen/setup
         txexpr
         "decoders.rkt")

(provide (all-defined-out))

(define (hyperlink-decoder inline-tx)
  (define (hyperlinker url . words)
    `(a [[href ,url] (class "align-text-bottom text-[#0077AA] no-underline hover:underline")]
        ,@words))

  (if (eq? 'hyperlink (get-tag inline-tx))
      (apply hyperlinker (get-elements inline-tx))
      inline-tx))

#|
`txt` is called by root when targeting LaTeX/PDF. It converts all elements inside
a ◊txt tag into a single concatenated string. ◊txt is not intended to be used in
normal markup; its sole purpose is to allow other tag functions to return LaTeX
code as a valid X-expression rather than as a string.
|#
(define (txt-decode xs)
  (if (member (get-tag xs) '(txt txt-noescape))
      (get-elements xs)
      xs))

; Escape $,%,# and & for LaTeX
; The approach here is rather indiscriminate; I’ll probably have to change
; it once I get around to handline inline math, etc.
; (define (ltx-escape-str str)
;   (regexp-replace* #px"([$#%&])" str "\\\\\\1"))

(define (ltx-escape-str str)
  (regexp-replace* #px"([#%$&])" str "\\\\\\1"))

(define (root . elements)
  (case (current-poly-target)
    [(tex pdf)
     (define first-pass
       (decode-elements elements
                        #:inline-txexpr-proc (compose1 txt-decode hyperlink-decoder)
                        #:string-proc (compose1 ltx-escape-str smart-quotes)
                        #:exclude-tags '(script style fig txt-noescape)))
     (make-txexpr 'body null (decode-elements first-pass #:inline-txexpr-proc txt-decode))]
    [(html)
     (define first-pass
       (decode-elements elements
                        #:txexpr-elements-proc (compose1 extract-divs-from-paragraphs
                                                         decode-paragraphs)
                        #:exclude-tags '(script style figure)))
     (define second-pass
       (decode-elements first-pass
                        #:inline-txexpr-proc hyperlink-decoder
                        #:string-proc smart-dashes
                        #:exclude-tags '(script style)))
     (define third-pass (decode-elements second-pass #:txexpr-proc extract-knowls))
     (make-txexpr 'body null third-pass)]))
