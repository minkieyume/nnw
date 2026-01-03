(define-module (nnw core serilize)
  #:use-module (nnw core generic)
  #:use-module (nnw core block)
  #:use-module (nnw core view)
  #:use-module (nnw core view document)
  #:use-module (nnw core view document org)
  #:use-module (oop goops)
  #:export (serilize
	    unserilize))

(define-generic serilize)

(define-generic unserilize)

(define-method (serilize (storable <storable>)))

(define (get-field name fields)
  (let ((pair (assoc name fields)))
    (if pair (cadr pair) #f)))

;; Serialize a block to S-expression format
(define-method (serilize (block <block>))
  (list 'block
        (list 'id (get-id block))
        (list 'description (get-description block))
        (list 'source (get-source block))
        (list 'type (get-type block))
        (list 'tags (get-tags block))
        (list 'hash (get-hash block))
        (list 'metadata (get-metadata block))
        (list 'created (get-created block))
        (list 'modified (get-modified block))))

(define (string->block-type type)
  (cond
   ((string=? type "text") <text>)
   ((string=? type "block") <block>)
   (else <block>)))

(define (unserilize/block data)
  (let* ((fields (cdr data))
         (block-id (get-field 'id fields))
         (block-type (get-field 'type fields)))
    (make (string->block-type block-type)
      #:id block-id
      #:description (get-field 'description fields)
      #:source (get-field 'source fields)
      #:tags (get-field 'tags fields)
      #:created (get-field 'created fields)
      #:modified (get-field 'modified fields)
      #:metadata (get-field 'metadata fields))))

;; Serialize a view to S-expression format
(define-method (serilize (view <view>))
  (list 'view
        (list 'id (get-id view))
        (list 'name (get-name view))
        (list 'type (get-type view))
        (list 'metadata (get-metadata view))
        (list 'content (get-content view))))

(define (string->view-type type)
  (cond
   ((string=? type "document") <document>)
   ((string=? type "view") <view>)
   (else <view>)))

;; Deserialize a view from S-expression format
(define (unserilize/view data)
  (let ((fields (cdr data)))
    (make (string->view-type (get-field 'type fields))
      #:id (get-field 'id fields)
      #:name (get-field 'name fields)
      #:metadata (get-field 'metadata fields)
      #:content (get-field 'content fields))))

(define-method (unserilize (data <list>))
  (cond ((eq? (car data) 'view) (unserilize/view data))
	((eq? (car data) 'block) (unserilize/block data))))
