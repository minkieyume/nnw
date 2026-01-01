(define-module (nnw core block)
  #:use-module (nnw core utils)
  #:use-module (oop goops)
  #:use-module (uuid generate)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 iconv)
  #:use-module (srfi srfi-1)
  #:use-module (gcrypt hash)
  #:export (<block>
            make-block
            block-id
            block-description
            block-source
            block-type
            block-tags
            block-hash
            block-created
            block-modified))

(define-class <block> ()
  (id #:init-keyword #:id 
      #:init-thunk generate-string-uuid
      #:getter block-id)
  (description #:init-keyword #:description #:getter block-description)
  (source #:init-keyword #:source #:getter block-source)
  (type #:init-keyword #:type #:getter block-type)
  (tags #:init-keyword #:tags #:getter block-tags)
  (hash #:init-keyword #:hash #:getter block-hash)
  (metadata #:init-keyword #:metadata #:getter view-metadata)
  (created #:init-keyword #:created #:getter block-created)
  (modified #:init-keyword #:modified #:getter block-modified))

;; ID Calculation: Compute the SHA-256 hash value from a string concatenated from the source and all tags.
;; TODO 参考view的格式将make-block方法重构为initialize方法，并修改可能波及到的单元测试。
(define* (make-block #:key
		     description
		     source
		     type
		     tags
		     created
		     modified
		     (metadata '()))
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
  (unless (and (list? metadata)
	       (every (lambda (key) (string? (car key))) metadata))
    (error (symbol->string 'metadata) " must be a Alist"))

  ;; Generate Hash
  (let* ((tags-str (string-join tags ","))
         (combined-str (string-append source tags-str))
         (hash-bytes (sha256 (string->utf8 combined-str)))
         ;; Convert bytevector to hexadecimal string representation
         (hash-hex (string-join
                    (map (lambda (byte)
                           (format #f "~2,'0x" byte))
                         (bytevector->u8-list hash-bytes))
                    "")))

    ;; Make Block
    (make <block>
      #:description description
      #:source source
      #:type type
      #:tags tags
      #:hash hash-hex
      #:metadata metadata
      #:created created
      #:modified modified)))
