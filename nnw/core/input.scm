(define-module (nnw core input)
  #:use-module (nnw core generic)
  #:use-module (nnw core storage)
  #:use-module (nnw core block)
  #:use-module (nnw core view)
  #:use-module (nnw core view document)
  #:use-module (nnw core utils)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (nnw-input))

;; Input entry point
(define* (nnw-input source #:key (storage (make <filest>))
		                 (parser '())
		                 (tags '())
                                 (view-id #f)
				 (view-type "document")
				 (view-name "Untitled Document")
				 (view-metadata '()))
  "Parse and store a view and its blocks"
  
  ;; Parse document source using parser module
  (let* ((parse-result (parse source <document>
                              (list #:tags tags
				    #:view-id view-id
				    #:view-name view-name
				    #:view-metadata view-metadata)))
         (doc (car parse-result))
         (blocks (cdr parse-result)))
    
    ;; Store blocks
    (for-each (lambda (block)
		(save block storage))
              blocks)
    
    ;; Store view
    (save doc storage)
    
    ;; Return view id
    (get-id doc)))
