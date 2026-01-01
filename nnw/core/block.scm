(define-module (nnw core block)
  #:use-module (nnw core utils)
  #:use-module (oop goops)
  #:use-module (uuid generate)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 iconv)
  #:use-module (srfi srfi-1)
  #:use-module (gcrypt hash)
  #:use-module (ice-9 optargs)
  #:export (<block>
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
  (metadata #:init-keyword #:metadata #:getter block-metadata)
  (created #:init-keyword #:created #:getter block-created)
  (modified #:init-keyword #:modified #:getter block-modified))

;; Type checking and initialization for block
(define-method (initialize (block <block>) initargs)
  (let-keywords initargs #f ((description #f)
                             (source #f)
                             (type #f)
                             (tags #f)
                             (created #f)
                             (modified #f)
                             (metadata '()))
    ;; Validate description
    (unless description
      (error "block description is required"))
    (unless (string? description)
      (error "block description must be a string" description))
    
    ;; Validate source
    (unless source
      (error "block source is required"))
    (unless (string? source)
      (error "block source must be a string" source))
    
    ;; Validate type
    (unless type
      (error "block type is required"))
    (unless (string? type)
      (error "block type must be a string" type))
    
    ;; Validate tags
    (unless tags
      (error "block tags is required"))
    (unless (list-of-string? tags)
      (error "block tags must be a list of strings" tags))
    
    ;; Validate created
    (unless created
      (error "block created is required"))
    (unless (string? created)
      (error "block created must be a string" created))
    
    ;; Validate modified
    (unless modified
      (error "block modified is required"))
    (unless (string? modified)
      (error "block modified must be a string" modified))
    
    ;; Validate metadata
    (unless (list? metadata)
      (error "block metadata must be a list" metadata))
    (unless (every (lambda (item)
                     (and (pair? item)
                          (string? (car item))))
                   metadata)
      (error "block metadata must be an alist with string keys" metadata))
    
    ;; Generate hash from source and tags
    (let* ((tags-str (string-join tags ","))
           (combined-str (string-append source tags-str))
           (hash-bytes (sha256 (string->utf8 combined-str)))
           ;; Convert bytevector to hexadecimal string representation
           (hash-hex (string-join
                      (map (lambda (byte)
                             (format #f "~2,'0x" byte))
                           (bytevector->u8-list hash-bytes))
                      "")))
      
      ;; Set the hash slot before calling next-method
      (slot-set! block 'hash hash-hex)))
  
  (next-method))
