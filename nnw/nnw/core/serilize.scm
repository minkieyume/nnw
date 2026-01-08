(define-module (nnw core serilize)
  #:use-module (nnw core generic)
  #:use-module (nnw core type)
  #:use-module (nnw core block)
  #:use-module (nnw core view)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
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
        (list 'content (get-content block))
        (list 'type (get-type block))
        (list 'tags (get-tags block))
        (list 'hash (get-hash block))
        (list 'metadata (get-metadata block))
        (list 'created (get-created block))
        (list 'modified (get-modified block))))

(define (unserilize/block data)
  (let* ((fields (cdr data))
         (block-id (get-field 'id fields))
         (block-type (get-field 'type fields)))
    (make (symbol->block-type block-type)
      #:id block-id
      #:description (get-field 'description fields)
      #:content (get-field 'content fields)
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

;; Deserialize a view from S-expression format
(define (unserilize/view data)
  (let ((fields (cdr data)))
    (make (symbol->view-type (get-field 'type fields))
      #:id (get-field 'id fields)
      #:name (get-field 'name fields)
      #:metadata (get-field 'metadata fields)
      #:content (get-field 'content fields))))

(define-method (unserilize (data <list>))
  (match (car data)
    ('view (unserilize/view data))
    ('block (unserilize/block data))))
