(define-module (nnw core block)
  #:use-module (nnw core generic)
  #:use-module (nnw core utils)
  #:use-module (oop goops)
  #:use-module (uuid generate)
  #:use-module (sxml match)
  #:use-module (sxml simple)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 optargs)
  #:export (<block>
	    <text>
            get-description
            get-tags
            get-hash
            get-created
            get-modified
	    unserilize/block
	    block-metadata-symbol-filter
	    input->block
	    input->block-content
	    filter-input-block))

(define-class <block-type> (<class>))

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
  (modified #:init-keyword #:modified #:getter get-modified)
  #:metaclass <block-type>)

(define-class <text-type> (<block-type>))

(define-class <text> (<block>)
  (type #:init-value 'text #:getter get-type)
  #:metaclass <text-type>)

(define (block-metadata-symbol-filter data)
  (not (member (car data) '(id type tags description created modified))))

(define (sxml->text-contents . input-sxmls)
  (map (lambda (input-sxml)
	 (sxml-match input-sxml
	    ((p . ,text) (apply string-append text))))
       input-sxmls))

;; TODO 分别针对默认的block，以及text类型的block，完成block->output的实现，要求转成下面这样的格式：
;; 	       (block (@ (id "xxxx-xxxx-xxxxx-xxxx(uuidv4)") ;optional
		      ;; 	 (type "text")
		      ;; 	 (tags "tag1 tag2") ;optional
		      ;; 	 (description "")   ;optional
		      ;; 	 (created "")       ;optional
		      ;; 	 (modified "")      ;optional
		      ;; 	 ;; ... metadata
		      ;; 	 )
		      ;; (p "text is there"))
(define-generic block->output)

(define-method (block->output (block <block>))
  (let ((id (get-id block))
        (type (symbol->string (get-type block)))
        (tags (string-join (get-tags block) " "))
        (description (get-description block))
        (created (get-created block))
        (modified (get-modified block))
        (metadata (get-metadata block))
        (content (get-content block)))
    `(block (@ (id ,id)
               (type ,type)
               (tags ,tags)
               (description ,description)
               (created ,created)
               (modified ,modified)
               ,@metadata)
            ,content)))

(define-method (block->output (text <text>))
  (let ((id (get-id text))
        (type (symbol->string (get-type text)))
        (tags (string-join (get-tags text) " "))
        (description (get-description text))
        (created (get-created text))
        (modified (get-modified text))
        (metadata (get-metadata text))
        (content (get-content text)))
    `(block (@ (id ,id)
               (type ,type)
               (tags ,tags)
               (description ,description)
               (created ,created)
               (modified ,modified)
               ,@metadata)
            (p ,content))))

(define-generic input->block-content) ;; block's children -> content

(define-method (input->block-content (input-contents <list>) (block-type <block-type>))
  (sxml->string input-contents))

(define-method (input->block-content (input-contents <list>) (block-type <text-type>))
  (apply string-append (apply sxml->text-contents input-contents)))

(define-generic input->block)

(define-method (input->block (input-block <list>) (block-type <block-type>))
  (sxml-match-let (((block (@ . ,meta) . ,children) input-block))
    (let ((id (assq-ref meta 'id))
	  (tags (assq-ref meta 'tags))
	  (description (assq-ref meta 'description))
	  (created (assq-ref meta 'created))
	  (modified (assq-ref meta 'modified))
	  (timestamp (current-timestamp))
	  (metadata (filter block-metadata-symbol-filter meta)))
      ;; (display "Debug: input-block = ")
      ;; (write input-block)
      ;; (newline)
      (apply make `(,block-type ,@(if id `(#:id ,(car id)) '())
				#:description ,(if description (car description) "")
				#:content ,(input->block-content children block-type)
				#:tags ,(if tags (string-split (car tags) #\space) '())
				#:modified ,(if modified (car modified) timestamp)
				#:created ,(if created (car created) timestamp)
				#:metadata ,metadata)))))

(define (filter-input-block input)
  (filter (lambda (block)
	    (if (equal? (car block) 'block)
		#t #f)) input))

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
