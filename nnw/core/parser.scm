(define-module (nnw core parser)
  #:use-module (nnw core generic)
  #:use-module (nnw core utils)
  #:use-module (nnw core view)
  #:use-module (oop goops)
  #:export ())

(define example-input-sxml
  '(view (@ (id "xxxx-xxxx-xxxxx-xxxx(uuidv4)")
	    (type "document")
	    (name "the view")
	    ;; ... metadata)
	    (block (@ (id "xxxx-xxxx-xxxxx-xxxx(uuidv4)") ;optional
		      (type "text")
		      (tags "tag1 tag2 tag3") ;optional
		      (description "")	      ;optional
		      (created "")	      ;optional
		      (modified "")	      ;optional
		      ;; ... metadata
		      )
		   (p "This is a text."))
	    (view (@ (id "xxxx-xxxx-xxxxx-xxxx(uuidv4)") (type "document")
		     ;; ... metadata
		     )
		  (block (@ (id "xxxx-xxxx-xxxxx-xxxx(uuidv4)") ;optional
			    (type "text")
			    (tags "tag1 tag2") ;optional
			    (description "")	      ;optional
			    (created "")	      ;optional
			    (modified "")	      ;optional
			    ;; ... metadata
			    )
			 (p "text is there"))))))
