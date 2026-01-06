(define-module (nnw core view)
  #:use-module (nnw core generic)
  #:use-module (nnw core utils)
  #:use-module (oop goops)
  #:use-module (uuid generate)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (sxml match)
  #:use-module (ice-9 optargs)
  #:export (<view>
	    <view-type>
	    get-name	    
	    view->output
	    make-content+blocks
	    sxml->view
	    valid-content?
	    view-metadata-symbol-filter
	    parse))

(define-class <view-type> (<class>))

(define-class <view> (<storable>)
  (id #:init-keyword #:id
      #:init-thunk generate-string-uuid
      #:getter get-id)
  (name #:init-keyword #:name #:getter get-name)
  (type #:init-value 'view #:getter get-type)
  (metadata #:init-keyword #:metadata #:init-value '() #:getter get-metadata)
  (content #:init-keyword #:content #:init-value '() #:getter get-content #:setter set-content!)
  #:metaclass <view-type>)

(define (view-metadata-symbol-filter data)
  (not (member (car data) '(id name type))))

(define-generic make-view)

(define-generic view->output)

(define-method (view->output (view <view>))
  (let ((id (get-id view))
        (name (get-name view))
        (metadata (get-metadata view))
        (content (get-content view)))
    `(view (@ (id ,id)
              (type "view")
              (name ,name)
              ,@(map (lambda (meta)
                       (list (car meta) (cdr meta)))
                     metadata))
           ,@(map (lambda (content-item)
                    `(ref (@ (id ,(car content-item))) ,@(cdr content-item)))
                  content))))

(define-generic make-content+blocks)

(define-method (make-content+blocks (raw-content <list>) (view-type <view-type>))
  '(() . ()))

(define-method (input->views+blocks (input <list>) (view-type <view-type>))
  (sxml-match-let (((view (@ . ,meta) . ,children) input))
     (let* ((id (assq-ref meta 'id))
	    (name (assq-ref meta 'name))
	    (metadata (filter view-metadata-symbol-filter meta))
	    (content+blocks (make-content+blocks children view-type))
	    (view (apply make `(,view-type
				,@(if id `(#:id ,(car id)) '())
				#:name ,(if name (car name) "")
				#:metadata ,metadata
				#:content ,(car content+blocks)))))
       (cons (list view) (cdr content+blocks)))))

;; Validate that content is an alist with UUID v4 string keys
(define (valid-content? content)
  (and (list? content)
       (every (lambda (item)
                (and (pair? item)
                     (uuid-v4-string? (car item))))
              content)))

;; Type checking for initialize method
(define-method (initialize (view <view>) initargs)  
  (let-keywords initargs #f ((id #f)
			     (name #f)
                             (metadata '())
                             (content '()))
    ;; Validate id
    (when id
      (unless (uuid-v4-string? id)
        (error "view id must be a valid UUID v4 string" id)))
    
    ;; Validate name
    (unless name
      (error "view name is required"))
    (unless (string? name)
      (error "view name must be a string" name))
    
    ;; Validate metadata
    (unless (list? metadata)
      (error "view metadata must be a list" metadata))
    
    ;; Validate content
    (unless (list? content)
      (error "view content must be a list" content))
    (unless (or (null? content)
		(valid-content? content))
      (error "view content must be an alist with UUID v4 string keys" content)))
  
  (next-method))
