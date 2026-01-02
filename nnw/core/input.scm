(define-module (nnw core input)
  #:use-module (nnw core block)
  #:use-module ((nnw core view) #:prefix nnw:)
  #:use-module (nnw core view document)
  #:use-module (nnw core parser)
  #:use-module (nnw core utils)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (nnw-input
            *view-storage*
            *block-storage*))

;; Global storage for views and blocks (in-memory for now)
(define *view-storage* (make-hash-table))
(define *block-storage* (make-hash-table))

;; Input entry point
(define* (nnw-input source 
                    #:key 
                    (tags '())
                    (view-id #f)
                    (view-type "document")
                    (view-name "Untitled Document")
                    (view-metadata '()))
  "Parse and store a document view with its blocks"
  
  (unless (string? source)
    (error "source must be a string" source))
  
  (unless (list-of-string? tags)
    (error "tags must be a list of strings" tags))
  
  (when view-id
    (unless (uuid-v4-string? view-id)
      (error "view-id must be a valid UUID v4 string" view-id)))
  
  ;; Parse document source using parser module
  (let* ((parse-result (parse source <document>
                              #:tags tags
                              #:view-id view-id
                              #:view-name view-name
                              #:view-metadata view-metadata))
         (doc (car parse-result))
         (blocks (cdr parse-result)))
    
    ;; Store blocks
    (for-each (lambda (block)
                (hash-set! *block-storage* (block-id block) block))
              blocks)
    
    ;; Store view
    (hash-set! *view-storage* (nnw:view-id doc) doc)
    
    ;; Return view id
    (nnw:view-id doc)))
