#lang racket/base

(require racket/string
         "config.rkt")

(provide (all-defined-out))

(define ($ . latex)
  `(script [(type "math/tex; mode=text")] ,(format "\\(~a\\)" (string-join latex ""))))

(define ($$ . latex)
  `(div [(class "math-container")]
        (div [(class "math-wrapper")]
             (script [(type "math/tex; mode=display")] ,(format "\\[~a\\]" (string-join latex ""))))))
