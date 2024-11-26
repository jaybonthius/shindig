#lang racket/base

(require pollen/setup
         racket/string
         "config.rkt")

(provide (all-defined-out))

(define ($ . latex)
  (case (current-poly-target)
    [(html) `(script [(type "math/tex; mode=text")] ,(format "~a" (string-join latex "")))]
    [(tex pdf) `(txt-noescape "$" ,@latex "$")]))

(define ($$ . latex)
  (case (current-poly-target)
    [(html)
     `(div [(class "math-container")]
           (div [(class "math-wrapper")]
                (script [(type "math/tex; mode=display")]
                        ,(format "~a" (string-join latex "")))))]
    [(tex pdf) `(txt-noescape "\\[" ,@latex "\\]")]))

(define (equation . latex)
  (case (current-poly-target)
    [(html)
     `(div [(class "math-container")]
           (div [(class "math-wrapper")]
                (script [(type "math/tex; mode=display")]
                        ,(format "\\begin{equation}~a\\end{equation}"
                                 (string-join latex "")))))]
    [(tex pdf) `(txt-noescape "\\begin{equation}" ,@latex "\\end{equation}")]))
