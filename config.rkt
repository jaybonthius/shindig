#lang racket/base

(provide (all-defined-out))

(define project-root (make-parameter (find-system-path 'orig-dir)))
(define (pollen-dir)
  (build-path (project-root)))
(define (sqlite-path)
  (build-path (pollen-dir) "sqlite"))

(define baseurl (make-parameter ""))
(define pretty-url (make-parameter ""))
