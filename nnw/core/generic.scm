(define-module (nnw core generic)
  #:use-module (oop goops)
  #:export (get-id
	    get-metadata
	    get-type
	    serilize
	    unserilize
	    save
	    read-from))

(define-generic get-id)

(define-generic get-metadata)

(define-generic get-type)

(define-generic serilize)

(define-generic unserilize)

(define-generic save)

(define-generic read-from)

(define-generic string->class)
