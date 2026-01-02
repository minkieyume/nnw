(define-module (nnw core view document)
  #:use-module (nnw core view)
  #:use-module (nnw core block)
  #:use-module (nnw core utils)
  #:use-module (oop goops)
  #:use-module (uuid generate)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:export (<document>
            valid-document-content?))

;; Document view: content is an alist of (uuid-string . index-number)
;; representing the order of blocks/views
(define-class <document> (<view>)
  (type #:init-value "document"))

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
  (let-keywords initargs #f ((content '()))
    ;; Validate document-specific content format
    (unless (or (null? content)
                (valid-document-content? content))
      (error "document content must be an alist with UUID v4 string keys and non-negative integer values" content)))
  
  (next-method))

;; Override view->string to output blocks/views in index order
(define-method (view->string (doc <document>))
  (let* ((content (view-content doc))
         ;; Sort by index value
         (sorted-content (sort content (lambda (a b) (< (cdr a) (cdr b)))))
         ;; Get block/view sources in order
         (sources (map (lambda (item)
                        (let ((id (car item)))
                          ;; Retrieve block or view by id and get its source/string
                          ;; For now, return empty string as placeholder
                          ""))
                      sorted-content)))
    (string-join sources "\n")))
