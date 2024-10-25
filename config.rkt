#lang racket/base

(provide (all-defined-out))

(define project-root (make-parameter (find-system-path 'orig-dir)))
(define (pollen-dir) (build-path (project-root) "content"))
(define (sqlite-path) (build-path (project-root) "sqlite"))

(define baseurl (make-parameter ""))
