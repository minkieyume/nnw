(use-modules (oop goops)
             (nnw core generic)
             (nnw core view)
             (nnw core view document)
             (nnw core block)
             (nnw core writer)
             (nnw core storage)
             (nnw core input)
             (sxml match)
             (srfi srfi-1)
             (srfi srfi-64))

(test-begin "logs/writer")

(test-group "write a view to text, get same value"
  (let* ((storage (make <filest> #:path "target/test"))
         (block1-id "550e8400-e29b-41d4-a716-446655440000")
         (block2-id "550e8400-e29b-41d4-a716-446655440001")
         (block1 (make <block> 
                   #:id block1-id
		   #:description ""
		   #:tags '("tag1")
		   #:modified "1111-111-111"
		   #:created "111-111-11"
                   #:content "This is a document source in Block 1."))
         (block2 (make <block>
                   #:id block2-id
		   #:description ""
		   #:tags '("tag1")
		   #:modified "1111-111-111"
		   #:created "111-111-11"
                   #:content "This is the second line in Block 2"))
         (test-sxml `(view (@ (id "test-view-id")
                             (type "view")
                             (name "Test View"))
                          (ref (@ (id ,block1-id)))
                          (ref (@ (id ,block2-id))))))
    
    ;; Save blocks to storage
    (save block1 storage)
    (save block2 storage)
    
    ;; Test write->text
    (test-equal "can write a output->text" 
                "This is a document source in Block 1.\nThis is the second line in Block 2"
                (write->text test-sxml storage))
    
    ;; Clean up test files
    (system "rm -rf target/test")))

(test-end "logs/writer")
