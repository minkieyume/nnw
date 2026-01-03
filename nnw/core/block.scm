(define-module (nnw core block)
  #:use-module (nnw core generic)
  #:use-module (nnw core utils)
  #:use-module (oop goops)
  #:use-module (uuid generate)  
  #:use-module (ice-9 iconv)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 optargs)
  #:export (<block>
	    <text>
            get-description
            get-source            
            get-tags
            get-hash
            get-created
            get-modified
	    unserilize/block))

(define-class <block> (<storable>)
  (id #:init-keyword #:id 
      #:init-thunk generate-string-uuid
      #:getter get-id)
  (description #:init-keyword #:description #:getter get-description)
  (source #:init-keyword #:source #:getter get-source)
  (type #:init-value "block" #:getter get-type)
  (tags #:init-keyword #:tags #:getter get-tags)
  (hash #:init-keyword #:hash #:getter get-hash)
  (metadata #:init-keyword #:metadata #:init-value '() #:getter get-metadata)
  (created #:init-keyword #:created #:getter get-created)
  (modified #:init-keyword #:modified #:getter get-modified))

(define-class <text> (<block>)
  (type #:init-value "text" #:getter get-type))

(define (block-type-checks initargs)
  (let-keywords initargs #f ((id #f)
			     (description #f)
                             (source #f)
                             (tags #f)
                             (created #f)
                             (modified #f)
                             (metadata '()))    
		;; Validate id
		(when id
		  (unless (uuid-v4-string? id)
		    (error "view id must be a valid UUID v4 string" id)))
		
		;; Validate description
		(unless description
		  (error "block description is required"))
		(unless (string? description)
		  (error "block description must be a string" description))
		
		;; Validate source
		(unless source
		  (error "block source is required"))
		(unless (string? source)
		  (error "block source must be a string" source))
		
		;; Validate tags
		(unless tags
		  (error "block tags is required"))
		(unless (list-of-string? tags)
		  (error "block tags must be a list of strings" tags))
		
		;; Validate created
		(unless created
		  (error "block created is required"))
		(unless (string? created)
		  (error "block created must be a string" created))
		
		;; Validate modified
		(unless modified
		  (error "block modified is required"))
		(unless (string? modified)
		  (error "block modified must be a string" modified))
		
		;; Validate metadata
		(unless (list? metadata)
		  (error "block metadata must be a list" metadata))
		(unless (every (lambda (item)
				 (and (pair? item)
				      (string? (car item))))
			       metadata)
		  (error "block metadata must be an alist with string keys" metadata))))

;; Type checking and initialization for block
(define-method (initialize (block <block>) initargs)
  (block-type-checks initargs)
  (let-keywords initargs #f ((id #f)
			     (description #f)
                             (source #f)
                             (type #f)
                             (tags #f)
                             (created #f)
                             (modified #f)
                             (metadata '()))
		;; Generate hash from source and tags
		(slot-set! block 'hash (apply generate-hash `("," ,source ,@tags))))
  (next-method))
