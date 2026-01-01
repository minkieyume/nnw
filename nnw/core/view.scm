(define-module (nnw core view)
  #:use-module (oop goops)
  #:use-module (uuid generate)
  #:export (<view>
	    view-id
	    view-name
	    view-metadata
	    view-content
	    view->string))

(define-class <view> ()
  (id #:init-keyword #:id
      #:init-thunk generate-string-uuid
      #:getter view-id)
  (name #:init-keyword #:name #:getter view-name)
  (metadata #:init-keyword #:metadata #:init-value '() #:getter view-metadata)
  (content #:init-keyword #:content #:init-value '() #:getter view-content))

;; Format a view to a string
(define-method (view->string (view <view>))
  (view-name view))

(define-method (initialize (view <view>) . initargs)
  (next-method))
