(define-module (nnw core block)
  #:use-module (oop goops)
  #:use-module (uuid generate)
  #:use-module (srfi srfi-1)
  #:export (<block>))

(define-class <block> ()
  id
  descrption
  source
  type
  tags
  created
  modified)

(define (list-of-string? lst)
  (and (list? lst)
       (every string? lst)))

;;TODO 将id赋值为：基于source和所有tags拼成的字符串计算的哈希值。
(define* (make-block #:key
		     description
		     source
		     type
		     tags
		     created
		     modified)
  ;; Type Checks
  (for-each
    (lambda (type)
      (unless (string? (eval type (interaction-environment)))
	(error (symbol->string type) " must be String!"))) 
    '(description source type created modified))
  (unless (list-of-string? tags)
    (error (symbol->string 'tags) " must be a List of String"))

  ;; Make Block
  (make <block>
    #:id ()
    #:description description
    #:source source
    #:type type
    #:created created
    #:modified modified))
