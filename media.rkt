#lang racket/base

(provide (all-defined-out))

(define (svg source . text)
  (define svg-src (string-append "../static/media/images/" source ".svg"))
  `(figure (object [[type "image/svg+xml"] [data ,svg-src]])))

(define (tldraw source . text)
  (define tldr-dark-src (string-append "../static/media/images/tldraw/light.svg/" source ".svg"))
  (define tldr-light-src (string-append "../static/media/images/tldraw/dark.svg/" source ".svg"))
  `(picture (source [[srcset ,tldr-dark-src] [media "(prefers-color-scheme: light)"]])
            (source [[srcset ,tldr-light-src] [media "(prefers-color-scheme: dark)"]])
            (img [[src ,tldr-light-src]])))
