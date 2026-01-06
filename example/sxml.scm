(define example-input-sxml
  '(view (@ (id "xxxx-xxxx-xxxxx-xxxx(uuidv4)")
	    (type "list-view")
	    (name "the view")
	    ;; ... metadata
	    )
	 (block (@ (id "xxxx-xxxx-xxxxx-xxxx(uuidv4)") ;optional
		   (type "text")
		   (tags "tag1 tag2 tag3") ;optional
		   (description "")	   ;optional
		   (created "")		   ;optional
		   (modified "")	   ;optional
		   ;; ... metadata
		   )
		(p "This is a text."))
	 (view (@ (id "xxxx-xxxx-xxxxx-xxxx(uuidv4)") (type "list-view")
		  ;; ... metadata
		  )
	       (block (@ (id "xxxx-xxxx-xxxxx-xxxx(uuidv4)") ;optional
			 (type "text")
			 (tags "tag1 tag2") ;optional
			 (description "")   ;optional
			 (created "")       ;optional
			 (modified "")      ;optional
			 ;; ... metadata
			 )
		      (p "text is there")))))

(define example-output-sxml
  '(view (@ (id "xxxx-xxxx-xxxxx-xxxx(uuidv4)")
	    (type "list-view")
	    (name "the view")
	    ;; ... metadata
	    )
	 (ref (@ (id "xxxx-xxxx-xxxxx-xxxx(uuidv4)")) "block1")
	 (ref (@ (id "xxxx-xxxx-xxxxx-xxxx(uuidv4)")) "type1")))
