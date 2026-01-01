(define-module (nnw core view)
  #:use-module (nnw core utils)
  #:use-module (oop goops)
  #:use-module (uuid generate)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 optargs)
  #:export (<view>
	    view-id
	    view-name
	    view-metadata
	    view-content
	    view->string
	    valid-content?))

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
