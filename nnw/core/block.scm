(define-module (nnw core block)
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

;; ID Calculation: Compute the SHA-256 hash value from a string concatenated from the source and all tags.
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

  ;; Generate ID
  (let* ((tags-str (string-join tags ","))
         (combined-str (string-append source tags-str))
         (hash-bytes (sha256 (string->utf8 combined-str)))
         ;; Convert bytevector to hexadecimal string representation
         (hash-hex (string-join
                    (map (lambda (byte)
                           (format #f "~2,'0x" byte))
                         (bytevector->u8-list hash-bytes))
                    ""))
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
