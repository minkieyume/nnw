(define-module (nnw core generic)
  #:use-module (oop goops)
  #:export (get-id
	    get-metadata
	    get-type
	    save
	    read-from
	    <storable>))

(define-generic get-id)

(define-generic get-metadata)

(define-generic get-type)

(define-generic save)

(define-generic read-from)

(define-class <storable> ()
  (id #:getter get-id))
