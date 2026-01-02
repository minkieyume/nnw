(define-module (nnw core output)
  #:use-module (nnw core block)
  #:use-module (nnw core view)
  #:use-module (nnw core view document)
  #:use-module (nnw core input)
  #:use-module (nnw core utils)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (nnw-output))

;; Retrieve block or view source by id
(define (get-source-by-id id)
  "Get the source content of a block or view by its id"
  (cond
   ;; Try to find in block storage
   ((hash-ref *block-storage* id)
    => (lambda (block) (block-source block)))
   ;; Try to find in view storage
   ((hash-ref *view-storage* id)
    => (lambda (view) (view->string view)))
   ;; Not found
   (else
    (error "Block or view not found with id" id))))

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
    
    ;; For document view, output blocks in order
    (if (is-a? view <document>)
        (let* ((content (view-content view))
               ;; Sort by index
               (sorted-content (sort content (lambda (a b) (< (cdr a) (cdr b)))))
               ;; Get sources
               (sources (map (lambda (item)
                              (get-source-by-id (car item)))
                            sorted-content)))
          (string-join sources "\n"))
        ;; For other view types, use view->string
        (view->string view))))
