(define-module (nnw core utils)
  #:use-module (srfi srfi-1)
  #:export (list-of-string?))

(define (list-of-string? lst)
  (and (list? lst)
       (every string? lst)))
