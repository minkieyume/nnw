(define-module (nnw core parser)
  #:use-module (nnw core generic)
  #:use-module (nnw core utils)
  #:use-module (nnw core view)
  #:use-module (nnw core type)
  #:use-module (oop goops)
  #:use-module (sxml match)
  #:use-module (srfi srfi-1)
  #:export (parse-text))

(define (fold-view-blocks . view-blocks)
  (fold (lambda (pair acc)
	  (cons (append (car pair) (car acc))
		(append (cdr pair) (cdr acc)))) '(() . ()) view-blocks))

(define (replace-input-view view)
  (if (equal? (car view) 'view)
      (sxml-match view
	((view (@ (id ,id) . ,otr) . ,chd)
	 `(view (@ (id ,id)))))
      view))

(define (replace-input-views input)
  (sxml-match-let (((view (@ . ,meta) . ,children)
		    input))
    (let ((modified-children (map replace-input-view children)))
      `(view (@ ,@meta) ,@modified-children))))

(define (view-filter view) ;; TODO 后期可能添加ID替代逻辑。
  (if (equal? (car view) 'view)
      #t
      #f))

(define (replace-children children)
  (map (lambda (view)
	 (input->views+blocks children))
       (filter view-filter children)))

(define-method (input->views+blocks (input <list>))
  (sxml-match-let (((view (@ (type ,type) . ,otr) . ,children) input))
    (let ((views-blocks (replace-children children)))
      (apply fold-view-blocks
	     (cons (input->views+blocks (replace-input-views input)
					(string->view-type type))
		   views-blocks)))))

(define (parse-text-block tags lines)
  (map (lambda (line)
    `(block (@ (type "text")
	       ,@(if (not (null? tags))
		     `((tags ,(string-join tags " ")))
		     '()))
	    (p ,line))) lines))

(define* (parse-text source #:key (tags '())
                                  (view-id #f)
				  (view-type "document")
				  (view-name "Untitled Document")
				  (view-metadata '()))
  (let* ((lines (filter (lambda (line) (not (string=? line "")))
			(string-split source #\newline)))
	 (blocks (parse-text-block tags lines)))
    `(view (@ (type ,view-type)
              (name ,view-name)
	      ,@view-metadata
	      ,@(if view-id `(id ,view-id) '()))
           ,@blocks)))
