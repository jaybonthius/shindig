#lang racket/base

(require (prefix-in config: "config.rkt"))

(provide (all-defined-out))

(define (svg source . text)
  (define svg-src (format "~astatic/media/images/~a.svg" (config:baseurl) source))
  `(figure (object [[type "image/svg+xml"] [data ,svg-src]])))

(define (tldraw source . text)
  (define tldr-dark-src
    (format "~astatic/media/images/tldraw/light.svg/~a.svg" (config:baseurl) source))
  (define tldr-light-src
    (format "~astatic/media/images/tldraw/dark.svg/~a.svg" (config:baseurl) source))
  `(picture (source [[srcset ,tldr-dark-src] [media "(prefers-color-scheme: light)"]])
            (source [[srcset ,tldr-light-src] [media "(prefers-color-scheme: dark)"]])
            (img [[src ,tldr-light-src]])))
