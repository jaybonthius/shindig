#lang racket/base

(require racket/list
         txexpr
         "utils.rkt")

(provide (all-defined-out))

(define (extract-divs-from-paragraphs elements)
  (append-map (λ (e)
                (cond
                  [(and (txexpr? e) (equal? (get-tag e) 'p))
                   ;; It's a paragraph; process its content to extract divs
                   (define-values (new-content divs) (extract-divs (get-elements e)))
                   ;; Reconstruct the paragraph with the new content
                   (if (empty? new-content)
                       divs ; If the paragraph is empty after extraction, skip it
                       (cons (txexpr 'p (get-attrs e) new-content) divs))]
                  ;; Not a paragraph; leave it as is
                  [else (list e)]))
              elements))

(define (extract-divs elems)
  ;; Recursively process elems, returning new content and extracted divs
  (define (loop es acc-content acc-divs)
    (cond
      [(empty? es) (values (reverse acc-content) (reverse acc-divs))]
      [(and (txexpr? (first es)) (equal? (get-tag (first es)) 'div))
       ;; Found a div; move it to acc-divs
       (loop (rest es) acc-content (cons (first es) acc-divs))]
      ;; Keep the element in acc-content
      [else (loop (rest es) (cons (first es) acc-content) acc-divs)]))
  (loop elems '() '()))

(define (extract-knowls x)
  (cond
    [(txexpr? x)
     (define tag (get-tag x))
     (define attrs (get-attrs x))
     (define elements (get-elements x))
     (define component-type (attr-ref attrs 'component-type #f))
     (define component-id (attr-ref attrs 'component-id #f))
     (cond
       [component-type
        (render-knowl (quote-xexpr-attributes (car elements))
                          (string->symbol component-type)
                          component-id)
        (txexpr tag
                (filter (λ (attr)
                          (and (not (equal? (car attr) 'component-type))
                               (not (equal? (car attr) 'component-id))))
                        attrs)
                (map extract-knowls elements))]
       [else (txexpr tag attrs (map extract-knowls elements))])]
    [else x]))
