(define-module (nnw core block)
  #:use-module (nnw core generic)
  #:use-module (nnw core utils)
  #:use-module (oop goops)
  #:use-module (uuid generate)
  #:use-module (ice-9 match)
  #:use-module (ice-9 iconv)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 optargs)
  #:export (<block>
	    <text>
            get-description
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
  (content #:init-keyword #:content #:getter get-content)
  (type #:init-value 'block #:getter get-type)
  (tags #:init-keyword #:tags #:getter get-tags)
  (hash #:init-keyword #:hash #:getter get-hash)
  (metadata #:init-keyword #:metadata #:init-value '() #:getter get-metadata)
  (created #:init-keyword #:created #:getter get-created)
  (modified #:init-keyword #:modified #:getter get-modified))

(define-class <text> (<block>)
  (type #:init-value 'text #:getter get-type))

(define-generic block->output)

(define (block-type-checks initargs)
  (let-keywords initargs #f ((id #f)
			     (description #f)
                             (content #f)
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
                             (content #f)
                             (type #f)
                             (tags #f)
                             (created #f)
                             (modified #f)
                             (metadata '()))
		;; Generate hash from content and tags
		(slot-set! block 'hash (apply generate-hash `("," ,content ,@tags))))
  (next-method))
