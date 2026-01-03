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
    (test-assert "id is generated" (string? (get-id block)))))

;; Test type validation
(test-group "type validation"
  (test-error "description must be string"
              (make <block>
               #:description 123
               #:source "source"
               #:tags '()
               #:created "2024-01-01"
               #:modified "2024-01-01"))
  
  (test-error "tags must be list of strings"
              (make <block>
               #:description "desc"
               #:source "source"
               #:tags "not-a-list"
               #:created "2024-01-01"
               #:modified "2024-01-01"))
  
  (test-error "created must be string"
              (make <block>
               #:description "desc"
               #:source "source"
               #:tags '()
               #:created 20240101
               #:modified "2024-01-01")))

;; Test serilize/unserilize
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
       #:modified "2024-01-01")))
  
  (test-assert "hash is generated" (string? (get-hash (make <block>
                                                        #:description "Hash test"
                                                        #:source "test-source"
                                                        #:tags '("tag1")
                                                        #:created "2024-01-01"
                                                        #:modified "2024-01-01")))))

(test-end "logs/block-tests")
