(define-module (nnw core parser)
  #:use-module (nnw core generic)
  #:use-module (nnw core utils)
  #:use-module (nnw core view)
  #:use-module (oop goops)
  #:export ())

(define example-output-sxml
  '(view (@ (id "xxxx-xxxx-xxxxx-xxxx(uuidv4)")
	    (type "document")
	    (name "the view")
	    ;; ... metadata
	    )
	 (block (@ (id "xxxx-xxxx-xxxxx-xxxx(uuidv4)")))
	 (view (@ (id "xxxx-xxxx-xxxxx-xxxx(uuidv4)")))))
