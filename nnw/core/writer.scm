(define-module (nnw core writer)
  #:use-module (nnw core generic)
  #:use-module (nnw core storage)
  #:use-module (nnw core utils)
  #:use-module (nnw core view)
  #:use-module (nnw core block)
  #:use-module (oop goops)
  #:use-module (sxml match)
  #:use-module (sxml simple)
  #:use-module (srfi srfi-1)
  #:export (write->text))

(define (id->output id storage)
  (let ((content (read-from id storage)))
    (cond
     ((is-a? content <block>) (block->output content))
     ((is-a? content <view>) (view->output content))
     (else
      (error "Block or View not found with id" id)))))

(define (sxml-content->string sxml)
  (cond ((string? sxml)
	 sxml)
	((list? sxml)
	 (sxml-match sxml
	   ((p . ,t) (apply string-append (map sxml-content->string t)))
	   (,otherwise "")))))

;; TODO 给该方法添加针对block的处理，同时更改该方法，确保能通过test/writer.scm的测试。
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
    ((block (@ . ,attrs) . ,content)
     (apply string-append (map sxml-content->string content)))
    (,otherwise "")))
