(define-module (nnw core view document)
  #:use-module (nnw core generic)
  #:use-module (nnw core view)
  #:use-module (nnw core block)
  #:use-module (nnw core block-type)
  #:use-module (nnw core utils)
  #:use-module (oop goops)
  #:use-module (sxml match)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:export (<document>
	    <document-type>
            valid-document-content?
	    make-document-blocks
	    make-document-contents
	    parse-document-source))

(define-class <document-type> (<view-type>))

;; Document view: content is an alist of (uuid-string . index-number)
;; representing the order of blocks/views
(define-class <document> (<view>)
  (type #:init-value 'document #:getter get-type)
  #:metaclass <document-type>)

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
  (let-keywords initargs #f ((id #f)
                             (name #f)
                             (metadata '())
                             (content '()))
    ;; Validate document-specific content format
    (unless (or (null? content)
                (valid-document-content? content))
      (error "document content must be an alist with UUID v4 string keys and non-negative integer values" content)))
  
  (next-method))

(define (doc-replace-child child)
  (sxml-match child
    ((ref (@ (id ,id)) . ,ot) `(ref (@ (id ,id))))
    (,oth child)))

(define (doc-context-sorter child-a child-b)
  (let* ((inxa (sxml-match child-a
	         ((ref (@ (id ,id)) ,inx . ,otr) inx)
		 (,oth 0)))
	 (inxb (sxml-match child-b
	         ((ref (@ (id ,id)) ,inx . ,otr) inx)
		 (,oth 1))))
    (< inxa inxb)))

;; Override view->string to output blocks/views in index order
(define-method (view->output (doc <document>))
  ;; (display "doc-content=")
  ;; (write (get-content doc))
  ;; (newline)
  (let ((id-list (map car (sort (get-content doc)
				(lambda (a b) (< (cdr a) (cdr b))))))
	(last-output (next-method)))
    (sxml-match last-output
       ((view (@ . ,m) . ,children)
	`(view (@ ,@m)
	       ,@(map doc-replace-child (sort children doc-context-sorter))))
       (,oth last-output))))

(define* (next-content+blocks children index next #:key (id #f) (block #f))
  (let ((next (cond (id (next (cdr children) (+ index 1)))
		    (else (next (cdr children) index))))
	(this (cond ((and id block) (cons (list (cons id index))
					  (list block)))
		    (id (cons (list (cons id index)) '()))
		    (else '()))))
    (if (null? next)
	this
	(if (null? this)
	    next
	    (fold-2lst-pairs this next)))))

(define* (input->content+blocks children #:optional (index 0))
  ;; (display "children=")
  ;; (write children)
  ;; (newline)
  (if (null? children) '()
      (let ((child (car children)))
	(sxml-match child
	   ((block (@ (id ,id) . ,otr) . ,chd) (next-content+blocks children index input->content+blocks #:id id))
	   ((block (@ . ,otr) . ,chd)
	    (let* ((block (input->block child)))
	      (next-content+blocks children index input->content+blocks #:id (get-id block) #:block block)))
	   (,otherwise (next-content+blocks children index input->content+blocks))))))

(define-method (make-content+blocks (raw-content <list>) (view-type <document-type>))
  (input->content+blocks raw-content))
