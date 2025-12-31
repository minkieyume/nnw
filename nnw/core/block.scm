(define-module (nnw core block)
  #:use-module (oop goops)
  #:use-module (uuid generate)
  #:use-module (srfi srfi-1)
  #:use-module (gcrypt hash)
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

;; 计算ID：基于source和所有tags拼成的字符串计算SHA-256哈希值
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

  ;; 生成ID
  (let* ((tags-str (string-join tags ","))
         (combined-str (string-append source tags-str))
         (hash-bytes (sha256 (string->utf8 combined-str)))
         (hash-hex (bytevector->base16-string hash-bytes))
         (id hash-hex))

    ;; Make Block
    (make <block>
      #:id id
      #:description description
      #:source source
      #:type type
      #:tags tags
      #:created created
      #:modified modified)))
