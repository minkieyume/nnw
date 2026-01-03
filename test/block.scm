(use-modules (oop goops)
	     (nnw core generic)
	     (nnw core serilize)
	     (nnw core block)
             (srfi srfi-64)
             (ice-9 regex))

(test-begin "logs/block-tests")

;; Test successful block creation
(test-group "make-block success cases"
  (let ((block (make <block>
                #:description "Test block"
                #:source "test-source.txt"
                #:tags '("tag1" "tag2")
                #:created "2024-01-01"
                #:modified "2024-01-02")))
    
    (test-assert "block is created" (is-a? block <block>))
    (test-equal "description is set" "Test block" (get-description block))
    (test-equal "source is set" "test-source.txt" (get-source block))
    (test-equal "tags are set" '("tag1" "tag2") (get-tags block))
    (test-equal "created is set" "2024-01-01" (get-created block))
    (test-equal "modified is set" "2024-01-02" (get-modified block))
    (test-assert "id is generated" (string? (get-id block)))
    (test-assert "id is non-empty" (> (string-length (get-id block)) 0))))

;; Test same sources produce different IDs
(test-group "ID generation uniqueness"
  (let* ((block1 (make <block>
                  #:description "Block 1"
                  #:source "source"
                  #:tags '("tag1")
                  #:created "2024-01-01"
                  #:modified "2024-01-01"))
         (block2 (make <block>
                  #:description "Block 2"
                  #:source "source"
                  #:tags '("tag1")
                  #:created "2024-01-01"
                  #:modified "2024-01-01")))
    
    (test-assert "different sources produce different IDs"
                 (not (string=? (get-id block1) (get-id block2))))))

;; Test empty tags list
(test-group "empty tags list"
  (let ((block (make <block>
                #:description "Block with no tags"
                #:source "source"
                #:tags '()
                #:created "2024-01-01"
                #:modified "2024-01-01")))
    
    (test-assert "block is created with empty tags" (is-a? block <block>))
    (test-equal "tags are empty" '() (get-tags block))
    (test-assert "id is still generated" (string? (get-id block)))))

;; Test type validation: description must be a string
(test-group "type validation - description"
  (test-error "description must be string"
              (make <block>
               #:description 123
               #:source "source"
               #:tags '()
               #:created "2024-01-01"
               #:modified "2024-01-01")))

;; Test type validation: tags must be a list of strings
(test-group "type validation - tags"
  (test-error "tags must be list of strings"
              (make <block>
               #:description "desc"
               #:source "source"
               #:tags "not-a-list"
               #:created "2024-01-01"
               #:modified "2024-01-01"))
  
  (test-error "tags list must contain only strings"
              (make <block>
               #:description "desc"
               #:source "source"
               #:tags '("tag1" 123 "tag2")
               #:created "2024-01-01"
               #:modified "2024-01-01")))

;; Test type validation: created must be a string
(test-group "type validation - created"
  (test-error "created must be string"
              (make <block>
               #:description "desc"
               #:source "source"
               #:tags '()
               #:created 20240101
               #:modified "2024-01-01")))

;; Test type validation: modified must be a string
(test-group "type validation - modified"
  (test-error "modified must be string"
              (make <block>
               #:description "desc"
               #:source "source"
               #:tags '()
               #:created "2024-01-01"
               #:modified 20240101)))

;; Test hash generation
(test-group "hash generation"
  (let ((block (make <block>
                #:description "Hash test"
                #:source "test-source"
                #:tags '("tag1" "tag2")
                #:created "2024-01-01"
                #:modified "2024-01-01")))
    
    (test-assert "hash is generated" (string? (get-hash block)))
    (test-assert "hash is non-empty" (> (string-length (get-hash block)) 0))
    (test-assert "hash is hexadecimal" 
                 (string-match "^[0-9a-f]+$" (get-hash block)))))

;; Test hash consistency
(test-group "hash consistency"
  (let* ((block1 (make <block>
                  #:description "Block 1"
                  #:source "same-source"
                  #:tags '("tag1" "tag2")
                  #:created "2024-01-01"
                  #:modified "2024-01-01"))
         (block2 (make <block>
                  #:description "Block 2"
                  #:source "same-source"
                  #:tags '("tag1" "tag2")
                  #:created "2024-01-01"
                  #:modified "2024-01-01")))
    
    (test-equal "same source and tags produce same hash"
                (get-hash block1)
                (get-hash block2))))

;; Test hash changes with different inputs
(test-group "hash changes with different inputs"
  (let* ((block1 (make <block>
                  #:description "Block 1"
                  #:source "source1"
                  #:tags '("tag1")
                  #:created "2024-01-01"
                  #:modified "2024-01-01"))
         (block2 (make <block>
                  #:description "Block 2"
                  #:source "source2"
                  #:tags '("tag1")
                  #:created "2024-01-01"
                  #:modified "2024-01-01"))
         (block3 (make <block>
                  #:description "Block 3"
                  #:source "source1"
                  #:tags '("tag2")
                  #:created "2024-01-01"
                  #:modified "2024-01-01")))
    
    (test-assert "different sources produce different hashes"
                 (not (string=? (get-hash block1) (get-hash block2))))
    (test-assert "different tags produce different hashes"
                 (not (string=? (get-hash block1) (get-hash block3))))))

;; Test metadata handling
(test-group "metadata handling"
  (let ((block (make <block>
                #:description "Metadata test"
                #:source "source"
                #:tags '("tag1")
                #:created "2024-01-01"
                #:modified "2024-01-01"
                #:metadata '(("key1" . "value1") ("key2" . "value2")))))
    
    (test-assert "block with metadata is created" (is-a? block <block>))
    (test-equal "metadata is set correctly"
                '(("key1" . "value1") ("key2" . "value2"))
                (get-metadata block))))

;; Test metadata validation
(test-group "metadata validation"
  (test-error "non-list metadata raises error"
              (make <block>
               #:description "desc"
               #:source "source"
               #:tags '("tag1")
               #:created "2024-01-01"
               #:modified "2024-01-01"
               #:metadata "not-a-list"))
  
  (test-error "metadata with non-string keys raises error"
              (make <block>
               #:description "desc"
               #:source "source"
               #:tags '("tag1")
               #:created "2024-01-01"
               #:modified "2024-01-01"
               #:metadata '((123 . "value")))))

;; Test required parameters
(test-group "required parameters"
  (test-error "missing description raises error"
              (make <block>
               #:source "source"
               #:tags '("tag1")
               #:created "2024-01-01"
               #:modified "2024-01-01"))
  
  (test-error "missing source raises error"
              (make <block>
               #:description "desc"
               #:tags '("tag1")
               #:created "2024-01-01"
               #:modified "2024-01-01"))
  
  (test-error "missing tags raises error"
              (make <block>
               #:description "desc"
               #:source "source"
               #:created "2024-01-01"
               #:modified "2024-01-01"))
  
  (test-error "missing created raises error"
              (make <block>
               #:description "desc"
               #:source "source"
               #:tags '("tag1")
               #:modified "2024-01-01"))
  
  (test-error "missing modified raises error"
              (make <block>
               #:description "desc"
               #:source "source"
               #:tags '("tag1")
               #:created "2024-01-01")))

(test-group "serilize/unserilize"
  
  (test-equal "can serilize"
    (serilize (make <block>
		#:id "54fe7506-d6b1-4bf5-96b4-4b5aeeabcfbb"
		#:description "desc"
		#:source "source"
		#:tags '("tag1")
		#:created "2024-01-01"
		#:modified "2024-01-01"))
    '(block
      (id "54fe7506-d6b1-4bf5-96b4-4b5aeeabcfbb")
      (description "desc")
      (source "source")
      (type "block")
      (tags ("tag1"))
      (hash "9da714991d02513b3a81e1e820a22e2ee841dc794ce74a796edb9eec51b7b94c")
      (metadata ())
      (created "2024-01-01")
      (modified "2024-01-01")))
  
  (test-equal "can unserilize"
    (get-hash
     (unserilize '(block
		   (id "54fe7506-d6b1-4bf5-96b4-4b5aeeabcfbb")
		   (description "desc")
		   (source "source")
		   (type "block")
		   (tags ("tag1"))
		   (hash "9da714991d02513b3a81e1e820a22e2ee841dc794ce74a796edb9eec51b7b94c")
		   (metadata ())
		   (created "2024-01-01")
		   (modified "2024-01-01"))))
    (get-hash
     (make <block>
       #:id "54fe7506-d6b1-4bf5-96b4-4b5aeeabcfbb"
       #:description "desc"
       #:source "source"
       #:tags '("tag1")
       #:created "2024-01-01"
       #:modified "2024-01-01"))))

(test-end "logs/block-tests")
