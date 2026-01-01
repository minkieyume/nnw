(use-modules (oop goops)
	     (nnw core block)
             (srfi srfi-64))

(test-begin "logs/block-tests")

;; Test successful block creation
(test-group "make-block success cases"
  (let ((block (make-block
                #:description "Test block"
                #:source "test-source.txt"
                #:type "text"
                #:tags '("tag1" "tag2")
                #:created "2024-01-01"
                #:modified "2024-01-02")))
    
    (test-assert "block is created" (is-a? block <block>))
    (test-equal "description is set" "Test block" (block-description block))
    (test-equal "source is set" "test-source.txt" (block-source block))
    (test-equal "type is set" "text" (block-type block))
    (test-equal "tags are set" '("tag1" "tag2") (block-tags block))
    (test-equal "created is set" "2024-01-01" (block-created block))
    (test-equal "modified is set" "2024-01-02" (block-modified block))
    (test-assert "id is generated" (string? (block-id block)))
    (test-assert "id is non-empty" (> (string-length (block-id block)) 0))))

;; Test same sources produce different IDs
(test-group "ID generation uniqueness"
  (let* ((block1 (make-block
                  #:description "Block 1"
                  #:source "source"
                  #:type "text"
                  #:tags '("tag1")
                  #:created "2024-01-01"
                  #:modified "2024-01-01"))
         (block2 (make-block
                  #:description "Block 2"
                  #:source "source"
                  #:type "text"
                  #:tags '("tag1")
                  #:created "2024-01-01"
                  #:modified "2024-01-01")))
    
    (test-assert "different sources produce different IDs"
                 (not (string=? (block-id block1) (block-id block2))))))

;; Test empty tags list
(test-group "empty tags list"
  (let ((block (make-block
                #:description "Block with no tags"
                #:source "source"
                #:type "text"
                #:tags '()
                #:created "2024-01-01"
                #:modified "2024-01-01")))
    
    (test-assert "block is created with empty tags" (is-a? block <block>))
    (test-equal "tags are empty" '() (block-tags block))
    (test-assert "id is still generated" (string? (block-id block)))))

;; Test type validation: description must be a string
(test-group "type validation - description"
  (test-error "description must be string"
              (make-block
               #:description 123
               #:source "source"
               #:type "text"
               #:tags '()
               #:created "2024-01-01"
               #:modified "2024-01-01")))

;; Test type validation: source must be a string
(test-group "type validation - source"
  (test-error "source must be string"
              (make-block
               #:description "desc"
               #:source 123
               #:type "text"
               #:tags '()
               #:created "2024-01-01"
               #:modified "2024-01-01")))

;; Test type validation: type must be a string
(test-group "type validation - type"
  (test-error "type must be string"
              (make-block
               #:description "desc"
               #:source "source"
               #:type 123
               #:tags '()
               #:created "2024-01-01"
               #:modified "2024-01-01")))

;; Test type validation: tags must be a list of strings
(test-group "type validation - tags"
  (test-error "tags must be list of strings"
              (make-block
               #:description "desc"
               #:source "source"
               #:type "text"
               #:tags "not-a-list"
               #:created "2024-01-01"
               #:modified "2024-01-01"))
  
  (test-error "tags list must contain only strings"
              (make-block
               #:description "desc"
               #:source "source"
               #:type "text"
               #:tags '("tag1" 123 "tag2")
               #:created "2024-01-01"
               #:modified "2024-01-01")))

;; Test type validation: created must be a string
(test-group "type validation - created"
  (test-error "created must be string"
              (make-block
               #:description "desc"
               #:source "source"
               #:type "text"
               #:tags '()
               #:created 20240101
               #:modified "2024-01-01")))

;; Test type validation: modified must be a string
(test-group "type validation - modified"
  (test-error "modified must be string"
              (make-block
               #:description "desc"
               #:source "source"
               #:type "text"
               #:tags '()
               #:created "2024-01-01"
               #:modified 20240101)))

(test-end "logs/block-tests")
