(define-module (nnw core utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 regex)
  #:export (list-of-string?
	    uuid-v4-string?))

(define (list-of-string? lst)
  (and (list? lst)
       (every string? lst)))

;; Check if a string is a valid UUID v4 format
(define (uuid-v4-string? str)
  (and (string? str)
       (let ((pattern "^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$"))
         (string-match pattern str))))
