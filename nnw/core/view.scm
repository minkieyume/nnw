(define-module (nnw core view)
  #:use-module (oop goops)
  #:use-module (uuid generate)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:export (<view>
	    view-id
	    view-name
	    view-metadata
	    view-content
	    view->string
	    valid-content?
	    uuid-v4-string?))

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

;; Check if a string is a valid UUID v4 format
(define (uuid-v4-string? str)
  (and (string? str)
       (let ((pattern "^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$"))
         (string-match pattern str))))

;; Validate that content is an alist with UUID v4 string keys
(define (valid-content? content)
  (and (list? content)
       (every (lambda (item)
                (and (pair? item)
                     (uuid-v4-string? (car item))))
              content)))

;; Type checking for initialize method
(define-method (initialize (view <view>) initargs)
  (let-keywords initargs #f ((name #f)
                             (metadata '())
                             (content '()))
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
    (when (and (not (null? content))
               (not (valid-content? content)))
      (error "view content must be an alist with UUID v4 string keys" content)))
  
  (next-method))
