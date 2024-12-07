#lang racket/base

(provide (all-defined-out))

(define project-root (make-parameter (find-system-path 'orig-dir)))
(define (pollen-dir)
  (build-path (project-root)))
(define (sqlite-path)
  (build-path (pollen-dir) "sqlite"))
(define (static-path)
  (build-path (pollen-dir) "static"))
(define (media-path)
  (build-path (static-path) "media"))
(define (images-path)
  (build-path (media-path) "images"))
(define (qr-codes-path)
  (build-path (images-path) "qr-codes")) ; TODO: make qr-codes path on build

(define baseurl (make-parameter ""))
(define pretty-url (make-parameter ""))
(define book-title (make-parameter ""))
(define author (make-parameter ""))
