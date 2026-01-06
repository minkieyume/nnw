(define-module (nnw core writer)
  #:use-module (nnw core generic)
  #:use-module (nnw core storage)
  #:use-module (nnw core utils)
  #:use-module (nnw core view)
  #:use-module (nnw core block)
  #:use-module (oop goops)
  #:use-module (sxml match)
  #:use-module (srfi srfi-1)
  #:export (write->text))

(define (id->output id storage)
  (let ((content (read-from id storage)))
    (cond
     ((is-a? content <block>) (block->output content))
     ((is-a? content <view>) (view->output content))
     (else
      (error "Block or View not found with id" id)))))

(define (write->text sxml storage)
  (sxml-match sxml
    ((view (@ . ,attrs) . ,children)
     (string-join
      (filter-map (lambda (child)
                    (sxml-match child
                      ((ref (@ (id ,id)))
		       (write->text (id->output id storage) storage))
                      (,otherwise #f)))
                  children)
      "\n"))
    (block)
    (,otherwise "")))
