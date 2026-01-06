(define-module (nnw core output)
  #:use-module (nnw core generic)
  #:use-module (nnw core storage)
  #:use-module (nnw core writer)
  #:use-module (nnw core view)
  #:use-module (nnw core utils)  
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (nnw-output))

;; Output a view as string
(define* (nnw-output view-id #:key (storage (make <filest>))
		                   (writer write->text))
  "Output a view by its id as a formatted string"
  (let ((view (read-from view-id storage)))
    (unless view
      (error "View not found with id" view-id))

    (let ((output (view->output view)))
      (writer output storage))))
