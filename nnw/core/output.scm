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

(define (id->output id backend)
  (let ((content (read-from id backend)))
    (cond
     ((is-a? content <block>) (get-source content))
     ((is-a? content <view>) (view->output content))
     (else
      (error "Block or View not found with id" id)))))

(define (id-list->string output backend)
  (string-join
   (map (lambda (x)
	  (cond
	   ((uuid-v4-string? x) (output->string (id->output x backend) backend))
	   (else x)))
	output)
   "\n"))

(define (output->string output backend)
  (cond ((string? output) output)
	((list-of-string? output) (id-list->string output backend))
	(else "")))

;; Output a view as string
(define* (nnw-output view-id #:key (backend (make <filest>)))
  "Output a view by its id as a formatted string"
  (let ((view (read-from view-id backend)))
    (unless view
      (error "View not found with id" view-id))

    (let ((output (view->output view)))
      (output->string output backend))))
