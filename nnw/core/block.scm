(define-module (nnw core block)
  #:use-module (oop goops)
  #:use-module (uuid generate)
  #:use-module (srfi srfi-1)
  #:use-module (gcrypt hash)
  #:export (<block>
            make-block
            block-id
            block-description
            block-source
            block-type
            block-tags
            block-created
            block-modified))

(define-class <block> ()
  (id #:init-keyword #:id #:getter block-id)
  (description #:init-keyword #:description #:getter block-description)
  (source #:init-keyword #:source #:getter block-source)
  (type #:init-keyword #:type #:getter block-type)
  (tags #:init-keyword #:tags #:getter block-tags)
  (created #:init-keyword #:created #:getter block-created)
  (modified #:init-keyword #:modified #:getter block-modified))

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
    (lambda (param-pair)
      (let ((name (car param-pair))
            (value (cdr param-pair)))
        (unless (string? value)
          (error (symbol->string name) " must be String!"))))
    `((description . ,description)
      (source . ,source)
      (type . ,type)
      (created . ,created)
      (modified . ,modified)))
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
