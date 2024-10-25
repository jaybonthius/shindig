#lang racket/base

(require racket/string
"config.rkt")

(provide (all-defined-out))

(define ($ . latex)
  `(script [(type "math/tex; mode=text")] ,(format "\\(~a\\)" (string-join latex ""))))

(define ($$ . latex)
  `(div [(class "flex justify-center")]
        (div [(class "inline-block")]
             (script [(type "math/tex; mode=display")] ,(format "\\[~a\\]" (string-join latex ""))))))
