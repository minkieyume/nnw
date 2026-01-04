(define-module (nnw core view document)
  #:use-module (nnw core generic)
  #:use-module (nnw core view)
  #:use-module (nnw core block)
  #:use-module (nnw core block-type)
  #:use-module (nnw core utils)
  #:use-module (oop goops)
  #:use-module (sxml match)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:export (<document>
	    <document-type>
            valid-document-content?
	    make-document-blocks
	    make-document-contents
	    parse-document-source))

(define-class <document-type> (<view-type>))

;; Document view: content is an alist of (uuid-string . index-number)
;; representing the order of blocks/views
(define-class <document> (<view>)
  (type #:init-value 'document #:getter get-type)
  #:metaclass <document-type>)

;; Validate that content is an alist with UUID v4 string keys and non-negative integer values
(define (valid-document-content? content)
  (and (list? content)
       (every (lambda (item)
                (and (pair? item)
                     (uuid-v4-string? (car item))
                     (integer? (cdr item))
                     (>= (cdr item) 0)))
              content)))

;; Override initialize to add document-specific content validation
(define-method (initialize (doc <document>) initargs)
  (let-keywords initargs #f ((id #f)
                             (name #f)
                             (metadata '())
                             (content '()))
    ;; Validate document-specific content format
    (unless (or (null? content)
                (valid-document-content? content))
      (error "document content must be an alist with UUID v4 string keys and non-negative integer values" content)))
  
  (next-method))

;; Override view->string to output blocks/views in index order
(define-method (view->output (doc <document>))
  (map car (sort (get-content doc)
		 (lambda (a b) (< (cdr a) (cdr b))))))

;; Parse document source into lines (helper function)
(define (parse-document-source source)
  "Split source by newline and filter empty lines."
  (let ((lines (string-split source #\newline)))
    (filter (lambda (s) (not (string=? s ""))) lines)))

(define* (make-document-blocks sources tags #:optional (index 0))
  (cond ((null? sources) '())
	(else
	 (let ((timestamp (current-timestamp)))
	   (cons
	    (make <text>
	      #:description (string-append "Block " (number->string (+ index 1)))
	      #:content (car sources)
	      #:tags tags
	      #:created timestamp
	      #:modified timestamp)
	    (make-document-blocks (cdr sources) tags (+ index 1)))))))

(define* (make-document-contents blocks #:optional (index 0))
  (cond ((null? blocks) '())
	(else
	 (cons
	  (cons (get-id (car blocks)) index)
	  (make-document-contents (cdr blocks) (+ index 1))))))

;; Parse source string into a document view and its blocks
(define-method (parse (source <string>) (view-class <document-type>) (parameters <list>))
  "Parse document source into a document view and its blocks.
   Parameters is a keyword list that may contain:
   - #:tags - list of tag strings
   - #:view-id - UUID v4 string for the view
   - #:view-name - name string for the view
   - #:view-metadata - metadata alist for the view
   Returns a pair (view . blocks)."
  
  (let-keywords parameters #f ((tags '())
                               (view-id #f)
                               (view-name "Untitled Document")
                               (view-metadata '()))
		
		(let* ((block-sources (parse-document-source source))
		       (blocks (make-document-blocks block-sources tags))
		       (content (make-document-contents blocks)))
		  
		  ;; Create view instance
		  (let ((view (make view-class
				#:id view-id
				#:name view-name
				#:metadata view-metadata
				#:content content)))
		    
		    ;; Return pair of (view . blocks)
		    (cons view blocks)))))

(define (input->block input-block)
  (sxml-match-let (((block (@ ,meta ...) ,children ...) input-block))
    (let ((id (assq-ref meta 'id))
	  (tags (assq-ref meta 'tags))
	  (description (assq-ref meta 'description))
	  (created (assq-ref meta 'created))
	  (modified (assq-ref meta 'modified))
	  (timestamp (current-timestamp))
	  (metadata (filter view-metadata-symbol-filter meta)))
      (make <text>
	#:id (if id id #f)
	#:description (if description description "")
	#:content (cadr children)
	#:tags (if tags (string-split tags #\space) '())
	#:modified (if modified modified timestamp)
	#:created (if created created timestamp)))))

(define-method (input->views+blocks (input <list>) (view-type <view-type>))
  (sxml-match-let (((view (@ ,meta ...) ,children ...) input))
     (let* ((id (assq-ref meta 'id))
	    (name (assq-ref meta 'name))
	    (metadata (filter view-metadata-symbol-filter meta))
	    (blocks (map input->block (filter-blocks children)))
	    (view (make view-type
		    #:id (if id id #f)
		    #:name (if name name "")
		    #:metadata (if metadata metadata '())
		    #:content (make-document-contents blocks))))
       (apply fold-view-blocks (cons (cons (list view) blocks)
				     (map (lambda (view-block) (cadr view-block))
					  (filter-view-blocks children)))))))
