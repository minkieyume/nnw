(define-module (nnw core output)
  #:use-module (nnw core generic)
  #:use-module (nnw core storage)
  #:use-module (nnw core block)
  #:use-module (nnw core view)
  #:use-module (nnw core view document)
  #:use-module (nnw core input)
  #:use-module (nnw core utils)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (nnw-output))

;; Output a view as string
(define (nnw-output view-id)
  "Output a view by its id as a formatted string"
  
  (unless (string? view-id)
    (error "view-id must be a string" view-id))
  
  (unless (uuid-v4-string? view-id)
    (error "view-id must be a valid UUID v4 string" view-id))
  
  (let ((view (hash-ref *view-storage* view-id)))
    (unless view
      (error "View not found with id" view-id))
    
    ;; Use view->string for all view types
    (view->string view)))
