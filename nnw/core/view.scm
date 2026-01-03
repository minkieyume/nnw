(define-module (nnw core view)
  #:use-module (nnw core generic)
  #:use-module (nnw core utils)
  #:use-module (oop goops)
  #:use-module (uuid generate)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 optargs)
  #:export (<view>
	    <view-type>
	    get-name
	    get-content
	    view->output
	    valid-content?
	    parse))

(define-class <view-type> (<class>))

(define-class <view> ()
  (id #:init-keyword #:id
      #:init-thunk generate-string-uuid
      #:getter get-id)
  (name #:init-keyword #:name #:getter get-name)
  (type #:init-value "view" #:getter get-type)
  (metadata #:init-keyword #:metadata #:init-value '() #:getter get-metadata)
  (content #:init-keyword #:content #:init-value '() #:getter get-content)
  #:metaclass <view-type>)

(define-generic view->output)

;; Format a view to a string
(define-method (view->output (view <view>))
  (get-name view))

(define-generic parse)

;; Parse a source string to a view
(define-method (parse (source <string>) (view-type <view-type>) (parameter <list>))
  (cons (make view-type #:name "View") '()))

(define-method (parse (source <string>) (view-type <view-type>))
  (parse source view-type '()))

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
