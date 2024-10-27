#lang racket/base

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

;; Import everything from all the files
(require "components.rkt"
         "config.rkt"
         "cross-references.rkt"
         "decode.rkt"
         "decoders.rkt"
         "math.rkt"
         "media.rkt"
         "miscellaneous.rkt"
         "sqlite.rkt"
         "tags.rkt"
         "utils.rkt")

;; Re-provide everything from those modules
(provide (all-from-out "components.rkt")
         (all-from-out "config.rkt")
         (all-from-out "cross-references.rkt")
         (all-from-out "decode.rkt")
         (all-from-out "decoders.rkt")
         (all-from-out "math.rkt")
         (all-from-out "media.rkt")
         (all-from-out "miscellaneous.rkt")
         (all-from-out "sqlite.rkt")
         (all-from-out "tags.rkt")
         (all-from-out "utils.rkt"))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (+ 2 2) 4))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline)
  (define who (box "world"))
  (command-line #:program "my-program"
                #:once-each [("-n" "--name") name "Who to say hello to" (set-box! who name)]
                #:args ()
                (printf "hello ~a~n" (unbox who))))
