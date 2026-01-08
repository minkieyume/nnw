(define-module (nnw core input)
  #:use-module (nnw core generic)
  #:use-module (nnw core storage)
  #:use-module (nnw core block)
  #:use-module (nnw core parser)
  #:use-module (nnw core view)
  #:use-module (nnw core view list-view)
  #:use-module (nnw core utils)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (nnw-input))

;; Input entry point
(define* (nnw-input source #:key (storage (make <filest>))
		                 (parser parse-text)
		                 (tags '())
                                 (view-id #f)
				 (view-type "list-view")
				 (view-name "Untitled List-View")
				 (view-metadata '()))
  "Parse and store a view and its blocks"
  
  ;; Parse list-view source using parser module
  (let* ((input (parser source #:tags tags
			       #:view-id view-id
			       #:view-type view-type
			       #:view-name view-name
			       #:view-metadata view-metadata))
	 (result (input->views+blocks input))
         (views (car result))
         (blocks (cdr result)))
    
    ;; Store blocks
    (for-each (lambda (block)
		(save block storage))
              blocks)
    
    ;; Store views
    (for-each (lambda (view)
		(save view storage))
              views)
    
    ;; Return view's ids
    (map get-id views)))
