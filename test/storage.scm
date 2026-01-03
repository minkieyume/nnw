(use-modules (oop goops)
             (nnw core generic)
             (nnw core block)
             (nnw core view)
             (nnw core storage)
	     (nnw core serilize)
             (srfi srfi-64)
             (ice-9 textual-ports)
             (ice-9 ftw))

(test-begin "logs/storage-tests")

;; Helper function to clean up test directories
(define (cleanup-test-storage path)
  (when (file-exists? path)
    (system* "rm" "-rf" path)))

;; Helper function to read file content
(define (read-file-content file-path)
  (call-with-input-file file-path
    (lambda (port)
      (read port))))

;; Test save view to file storage
(test-group "save view to file storage"
  (let* ((storage (make <filest> #:path "target/test-storage-view"))
         (view (make <view>
                 #:id "550e8400-e29b-41d4-a716-446655440000"
                 #:name "Test View"
                 #:metadata '(("author" . "tester"))
                 #:content '(("550e8400-e29b-41d4-a716-446655440001" . "block1"))))
         (expected-file "target/test-storage-view/view/550e8400-e29b-41d4-a716-446655440000.scm"))
    
    (cleanup-test-storage "target/test-storage-view")
    (save view storage)
    
    (test-assert "view file is created" (file-exists? expected-file))
    (test-equal "saved data matches serialized view"
                (serilize view)
                (read-file-content expected-file))
    (test-assert "view directory exists" (file-exists? "target/test-storage-view/view"))
    
    (cleanup-test-storage "target/test-storage-view")))

;; Test save block to file storage
(test-group "save block to file storage"
  (let* ((storage (make <filest> #:path "target/test-storage-block"))
         (block (make <block>
                  #:id "550e8400-e29b-41d4-a716-446655440010"
                  #:description "Test Block"
                  #:source "test source content"
                  #:tags '("tag1" "tag2")
                  #:created "2024-01-01"
                  #:modified "2024-01-02"))
         (expected-file "target/test-storage-block/block/550e8400-e29b-41d4-a716-446655440010.scm"))
    
    (cleanup-test-storage "target/test-storage-block")
    (save block storage)
    
    (test-assert "block file is created" (file-exists? expected-file))
    (test-equal "saved data matches serialized block"
                (serilize block)
                (read-file-content expected-file))
    (test-assert "block directory exists" (file-exists? "target/test-storage-block/block"))
    
    (cleanup-test-storage "target/test-storage-block")))

;; Test read-from with view
(test-group "read-from view"
  (let* ((storage (make <filest> #:path "target/test-storage-read-view"))
         (view-id "550e8400-e29b-41d4-a716-446655440100")
         (original-view (make <view>
                          #:id view-id
                          #:name "Test Read View"
                          #:metadata '(("author" . "tester"))
                          #:content '(("550e8400-e29b-41d4-a716-446655440101" . "block1")))))
    
    (cleanup-test-storage "target/test-storage-read-view")
    (save original-view storage)
    
    (let ((read-view (read-from view-id storage)))
      (test-assert "read-from returns a view object" (is-a? read-view <view>))
      (test-equal "read view has correct id" view-id (get-id read-view))
      (test-equal "read view has correct name" "Test Read View" (get-name read-view)))
    
    (cleanup-test-storage "target/test-storage-read-view")))

;; Test read-from with block
(test-group "read-from block"
  (let* ((storage (make <filest> #:path "target/test-storage-read-block"))
         (block-id "550e8400-e29b-41d4-a716-446655440110")
         (original-block (make <block>
                           #:id block-id
                           #:description "Test Read Block"
                           #:source "test source content"
                           #:tags '("tag1" "tag2")
                           #:created "2024-01-01"
                           #:modified "2024-01-02"
                           #:metadata '(("key" . "value")))))
    
    (cleanup-test-storage "target/test-storage-read-block")
    (save original-block storage)
    
    (let ((read-block (read-from block-id storage)))
      (test-assert "read-from returns a block object" (is-a? read-block <block>))
      (test-equal "read block has correct id" block-id (get-id read-block))
      (test-equal "read block has correct description" "Test Read Block" (get-description read-block)))
    
    (cleanup-test-storage "target/test-storage-read-block")))

;; Test round-trip: save and read-from
(test-group "round-trip save and read-from"
  (let* ((storage (make <filest> #:path "target/test-storage-roundtrip"))
         (view-id "550e8400-e29b-41d4-a716-446655440140")
         (block-id "550e8400-e29b-41d4-a716-446655440141")
         (original-view (make <view>
                          #:id view-id
                          #:name "Roundtrip View"
                          #:metadata '(("key1" . "value1") ("key2" . "value2"))
                          #:content '(("550e8400-e29b-41d4-a716-446655440142" . "b1"))))
         (original-block (make <block>
                           #:id block-id
                           #:description "Roundtrip Block"
                           #:source "roundtrip source"
                           #:tags '("round" "trip")
                           #:created "2024-01-01"
                           #:modified "2024-01-02")))
    
    (cleanup-test-storage "target/test-storage-roundtrip")
    (save original-view storage)
    (save original-block storage)
    
    (let ((read-view (read-from view-id storage))
          (read-block (read-from block-id storage)))
      (test-equal "view name matches after round-trip" 
                  (get-name original-view) 
                  (get-name read-view))
      (test-equal "block description matches after round-trip" 
                  (get-description original-block) 
                  (get-description read-block))
      (test-equal "view metadata matches after round-trip" 
                  (get-metadata original-view) 
                  (get-metadata read-view)))
    
    (cleanup-test-storage "target/test-storage-roundtrip")))

;; Test read-from with non-existent id
(test-group "read-from non-existent id"
  (let* ((storage (make <filest> #:path "target/test-storage-read-notfound"))
         (non-existent-id "550e8400-e29b-41d4-a716-999999999999"))
    
    (cleanup-test-storage "target/test-storage-read-notfound")
    
    (let ((result (read-from non-existent-id storage)))
      (test-assert "read-from returns #f for non-existent id" (not result)))
    
    (cleanup-test-storage "target/test-storage-read-notfound")))

(test-end "logs/storage-tests")
