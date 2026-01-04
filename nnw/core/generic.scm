(define-module (nnw core generic)
  #:use-module (oop goops)
  #:export (get-id
	    get-metadata
	    get-type
	    get-content
	    save
	    read-from
	    <storable>))

(define-generic get-id)

(define-generic get-metadata)

(define-generic get-type)

(define-generic get-content)

(define-generic save)

(define-generic read-from)

(define-generic input->views+blocks)

(define-class <storable> ()
  (id #:getter get-id))
