(define-module (nnw core view document)
  #:use-module (nnw core generic)
  #:use-module (nnw core storage)
  #:use-module (nnw core view)
  #:use-module (nnw core block)
  #:use-module (nnw core utils)
  #:use-module (oop goops)
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
  (type #:init-value "document")
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
(define-method (view->string (doc <document>))
  (let* ((content (get-content doc))
         ;; Sort by index value
         (sorted-content (sort content (lambda (a b) (< (cdr a) (cdr b)))))
         ;; Get block/view sources in order
         (sources (map (lambda (item)
                        (let ((id (car item)))
                          ;; Retrieve block or view by id and get its source/string
                          (cond
                           ((hash-ref *block-storage* id)
                            => (lambda (block) (get-source block)))
                           ((hash-ref *view-storage* id)
                            => (lambda (view) (view->string view)))
                           (else
                            (error "Block or view not found with id" id)))))
                      sorted-content)))
    (string-join sources "\n")))

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
	      #:source (car sources)
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
