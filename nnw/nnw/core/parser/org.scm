(define-module (nnw core parser org)
  #:use-module (nnw core generic)
  #:use-module (nnw core utils)
  #:use-module (nnw core view)
  #:use-module (nnw core type)
  #:use-module (orgfile)
  #:use-module (orgfile node)
  #:use-module (oop goops)
  #:use-module (sxml match)
  #:use-module (srfi srfi-1)
  #:export (parse-org))

(define* (document->input d)
  (if (document-node? d)
      (fold+convert (node-children d))
      (error "not a document node")))

(define (node->input n tags not-block)
  (cond ((section-node? n) (section-node->input n))
        ((list-node? n) (list-node->input n tags))
        ((item-node? n) (item-node->input n))
        ((paragraph-node? n) (paragraph-node->input n tags not-block))
        ((drawer-node? n) (drawer-node->input n))
        ((link-node? n) (link-node->input n))
        ((text-node? n) (text-node->input n))
        (else (error "unrecognized node"))))

(define (properties-drawer? n)
  (and (drawer-node? n)
       (equal? (node-get-data n 'name) "PROPERTIES")))

(define (block-drawer? n)
  (and (drawer-node? n)
       (equal? (node-get-data n 'name) "BLOCK")))

(define (get-section-properties n)
  (let ((fn (filter properties-drawer? (node-children n))))
    (if (null? fn)
	'()
	(drawer-get-metadata (car fn)))))

(define (has-no-child-section? n)
  (every (compose not section-node?) (node-children n)))

;; 问题解决思路：Section里面的PROPERTIES Drawer修饰View元数据。
;; 而每个BLOCK前的BLOCK Drawer修饰单个Block的元数据
(define* (section-node->input n)
  (let* ((level (node-get-data n 'level))
         (headline (node-get-data n 'headline))
         (tags (node-get-data n 'tags)))
    `(view (@ (name ,headline)
	      (level ,(number->string level))
	      (tags ,(string-join tags " "))
	      ,@(map (lambda (p) `(,(car p) ,(cdr p)))
		     (get-section-properties n)))
	   ,@(fold+convert (filter (compose not properties-drawer?) (node-children n)) #:tags tags))))

(define* (list-node->input n tags)
  (let* ((ordered? (node-get-data n 'ordered))
         (list-type (if ordered? 'ol 'ul)))
    `(block (@ (tags ,(string-join tags " "))) (,list-type ,(fold+convert (node-children n) #:not-block #t)))))

(define* (item-node->input n)
  `(li ,(fold+convert (node-children n) #:not-block #t)))

(define* (paragraph-node->input n tags not-block)
  (let ((folded (fold+convert (node-children n))))
    (if not-block
	`(p ,@folded)
	`(block (@ (tags ,(string-join tags " "))) (p ,@folded)))))

(define* (drawer-node->input n)
  ;; (if (block-drawer? n)
  ;;     #f
  ;;     (let* ((name (node-get-data n 'name))
  ;;            (metadata (drawer-get-metadata n))
  ;;            (children (node-children n))
  ;;            (dl-content (map (lambda (pair)
  ;; 				`((dt ,(symbol->string (car pair)))
  ;; 				  (dd ,(cdr pair))))
  ;;                             metadata)))
  ;; 	`(details (summary ,name)
  ;; 		  (dl ,@(apply append dl-content))
  ;; 		  ,@(fold+convert children))))
  #f )

(define* (link-node->input n)
  (let ((url (node-get-data n 'url))
        (desc (node-get-data n 'description)))
    `(a (@ (href ,url)) ,desc)))

(define* (text-node->input n)
  (string-join (reverse (node-children n))))

(define (merge-tags string-tags tags-list)
  (string-join (lset-union equal? (string-split string-tags #\space) tags-list) " "))

(define (k-pair->k-list pair)
  (list (car pair) (cdr pair)))

(define (block-type block)
  (sxml-match-let (((block (@ . ,mta) . ,children) block))
    (cond ((every (lambda (chd) (every string? (cdr chd))) children) "text")
	  (else "sxml"))))

(define (block-sxml? n)
  (if (eq? (car n) 'block) #t #f))

(define (add-type block)
  (sxml-match block
    ((block (@ (type ,type) . ,meta) . ,chd) block)
    ((block (@ . ,meta) . ,chd) `(block (@ (type ,(block-type block)) ,@meta) ,@chd))
    ((block . ,chd) `(block (@ (type ,(block-type block))) ,@chd))
    (,oth block)))

(define (edit-prev-block-with-drawer cur prev)
  (if (null? prev)
      prev
      (let* ((name (node-get-data cur 'name))
             (metadata (drawer-get-metadata cur))
	     (tgs (assq-ref metadata 'tags))
             (children (node-children cur))
	     (block (car prev)))
	(cons
	 (add-type
	  (sxml-match block
	    ((block (@ (tags ,tags) . ,meta) . ,chd)
	     `(block (@ (tags ,(merge-tags tags (if tgs (string-split tgs #\space) '())))
		       ,@meta
		       ,@(map k-pair->k-list (delete (assq 'tags metadata) metadata))) ,@children ,@chd))
	    ((block (@ . ,meta) . ,chd)
	     `(block (@ ,@meta
			,@(map k-pair->k-list metadata)) ,@children ,@chd))
	    ((block . ,chd)
	     `(block (@ ,@(map k-pair->k-list metadata)) ,@children ,@chd))
	    (,oth oth)))
	 (cdr prev)))))

(define (edit-car proc lst)
  (cons (proc (car lst)) (cdr lst)))

(define (edit-prev-block prev)
  (let ((block (car prev)))
    (cons
     (add-type block)
     (cdr prev))))

(define (edit-prev cur prev)
  (cond ((null? prev) prev) 
	((block-drawer? cur) (edit-prev-block-with-drawer cur prev))
	((block-sxml? (car prev)) (edit-car add-type prev))
	(else prev)))

(define* (fold+convert lst #:key (tags '()) (not-block #f))
  (define (convert+cons elem prev)
    (let ((result (node->input elem tags not-block))
	  (pv (edit-prev elem prev)))
      (if result
	  (cons result pv)
	  pv)))
  (edit-car (lambda (x) (if (and (list? x)
				 (block-sxml? x)) (add-type x) x))
	    (fold convert+cons '() lst)))

(define* (parse-org source #:key (tags '())
                                 (view-id #f)
				 (view-type "list-view")
				 (view-name "Untitled Document")
				 (view-metadata '()))
  (let* ((org-file (parse-orgfile source))
	 (before-input (document->input org-file)))
    before-input))
