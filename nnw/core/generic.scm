(define-module (nnw core generic)
  #:use-module (oop goops)
  #:export (get-id
	    get-metadata
	    get-type))

(define-generic get-id)

(define-generic get-metadata)

(define-generic get-type)
