(use-modules (oop goops)
             (nnw core generic)
             (nnw core block)
             (nnw core view)
             (nnw core storage)
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
    
    ;; Clean up before test
    (cleanup-test-storage "target/test-storage-view")
    
    ;; Save view
    (save view storage)
    
    ;; Test file exists
    (test-assert "view file is created" (file-exists? expected-file))
    
    ;; Test file content
    (let ((saved-data (read-file-content expected-file)))
      (test-equal "saved data matches serialized view"
                  (serilize view)
                  saved-data))
    
    ;; Clean up after test
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
    
    ;; Clean up before test
    (cleanup-test-storage "target/test-storage-block")
    
    ;; Save block
    (save block storage)
    
    ;; Test file exists
    (test-assert "block file is created" (file-exists? expected-file))
    
    ;; Test file content
    (let ((saved-data (read-file-content expected-file)))
      (test-equal "saved data matches serialized block"
                  (serilize block)
                  saved-data))
    
    ;; Clean up after test
    (cleanup-test-storage "target/test-storage-block")))

;; Test save text block to file storage
(test-group "save text block to file storage"
  (let* ((storage (make <filest> #:path "target/test-storage-text"))
         (text-block (make <text>
                       #:id "550e8400-e29b-41d4-a716-446655440020"
                       #:description "Test Text Block"
                       #:source "text content"
                       #:tags '("text" "test")
                       #:created "2024-01-01"
                       #:modified "2024-01-02"))
         (expected-file "target/test-storage-text/block/550e8400-e29b-41d4-a716-446655440020.scm"))
    
    ;; Clean up before test
    (cleanup-test-storage "target/test-storage-text")
    
    ;; Save text block
    (save text-block storage)
    
    ;; Test file exists
    (test-assert "text block file is created" (file-exists? expected-file))
    
    ;; Test file content
    (let ((saved-data (read-file-content expected-file)))
      (test-equal "saved data matches serialized text block"
                  (serilize text-block)
                  saved-data))
    
    ;; Clean up after test
    (cleanup-test-storage "target/test-storage-text")))

;; Test directory creation
(test-group "directory creation"
  (let* ((storage (make <filest> #:path "target/test-storage-dirs"))
         (view (make <view>
                 #:id "550e8400-e29b-41d4-a716-446655440030"
                 #:name "Dir Test View"))
         (block (make <block>
                  #:id "550e8400-e29b-41d4-a716-446655440031"
                  #:description "Dir Test Block"
                  #:source "content"
                  #:tags '("test")
                  #:created "2024-01-01"
                  #:modified "2024-01-01")))
    
    ;; Clean up before test
    (cleanup-test-storage "target/test-storage-dirs")
    
    ;; Save view (should create view directory)
    (save view storage)
    (test-assert "view directory is created" 
                 (file-exists? "target/test-storage-dirs/view"))
    
    ;; Save block (should create block directory)
    (save block storage)
    (test-assert "block directory is created" 
                 (file-exists? "target/test-storage-dirs/block"))
    
    ;; Clean up after test
    (cleanup-test-storage "target/test-storage-dirs")))

;; Test multiple saves to same storage
(test-group "multiple saves to same storage"
  (let* ((storage (make <filest> #:path "target/test-storage-multiple"))
         (view1 (make <view>
                  #:id "550e8400-e29b-41d4-a716-446655440040"
                  #:name "View 1"))
         (view2 (make <view>
                  #:id "550e8400-e29b-41d4-a716-446655440041"
                  #:name "View 2"))
         (block1 (make <block>
                   #:id "550e8400-e29b-41d4-a716-446655440042"
                   #:description "Block 1"
                   #:source "content1"
                   #:tags '("test")
                   #:created "2024-01-01"
                   #:modified "2024-01-01"))
         (block2 (make <block>
                   #:id "550e8400-e29b-41d4-a716-446655440043"
                   #:description "Block 2"
                   #:source "content2"
                   #:tags '("test")
                   #:created "2024-01-01"
                   #:modified "2024-01-01")))
    
    ;; Clean up before test
    (cleanup-test-storage "target/test-storage-multiple")
    
    ;; Save multiple items
    (save view1 storage)
    (save view2 storage)
    (save block1 storage)
    (save block2 storage)
    
    ;; Test all files exist
    (test-assert "view1 file exists" 
                 (file-exists? "target/test-storage-multiple/view/550e8400-e29b-41d4-a716-446655440040.scm"))
    (test-assert "view2 file exists" 
                 (file-exists? "target/test-storage-multiple/view/550e8400-e29b-41d4-a716-446655440041.scm"))
    (test-assert "block1 file exists" 
                 (file-exists? "target/test-storage-multiple/block/550e8400-e29b-41d4-a716-446655440042.scm"))
    (test-assert "block2 file exists" 
                 (file-exists? "target/test-storage-multiple/block/550e8400-e29b-41d4-a716-446655440043.scm"))
    
    ;; Clean up after test
    (cleanup-test-storage "target/test-storage-multiple")))

;; Test overwriting existing file
(test-group "overwrite existing file"
  (let* ((storage (make <filest> #:path "target/test-storage-overwrite"))
         (view-id "550e8400-e29b-41d4-a716-446655440050")
         (view1 (make <view>
                  #:id view-id
                  #:name "Original View"))
         (view2 (make <view>
                  #:id view-id
                  #:name "Updated View"))
         (file-path (string-append "target/test-storage-overwrite/view/" view-id ".scm")))
    
    ;; Clean up before test
    (cleanup-test-storage "target/test-storage-overwrite")
    
    ;; Save first version
    (save view1 storage)
    (let ((first-save (read-file-content file-path)))
      (test-equal "first save has original name"
                  "Original View"
                  (cadr (assoc 'name (cdr first-save)))))
    
    ;; Save second version (overwrite)
    (save view2 storage)
    (let ((second-save (read-file-content file-path)))
      (test-equal "second save has updated name"
                  "Updated View"
                  (cadr (assoc 'name (cdr second-save)))))
    
    ;; Clean up after test
    (cleanup-test-storage "target/test-storage-overwrite")))

;; Test custom storage path
(test-group "custom storage path"
  (let* ((custom-path "target/custom-storage-path")
         (storage (make <filest> #:path custom-path))
         (view (make <view>
                 #:id "550e8400-e29b-41d4-a716-446655440060"
                 #:name "Custom Path View")))
    
    ;; Clean up before test
    (cleanup-test-storage custom-path)
    
    ;; Save view
    (save view storage)
    
    ;; Test file exists in custom path
    (test-assert "file exists in custom path"
                 (file-exists? (string-append custom-path "/view/550e8400-e29b-41d4-a716-446655440060.scm")))
    
    ;; Clean up after test
    (cleanup-test-storage custom-path)))

;; Test get-path method
(test-group "get-path method"
  (let ((storage1 (make <filest>))
        (storage2 (make <filest> #:path "custom/path")))
    
    (test-equal "default path is target/storage"
                "target/storage"
                (get-path storage1))
    
    (test-equal "custom path is preserved"
                "custom/path"
                (get-path storage2))))

;; Test read-from with view
(test-group "read-from view"
  (let* ((storage (make <filest> #:path "target/test-storage-read-view"))
         (view-id "550e8400-e29b-41d4-a716-446655440100")
         (original-view (make <view>
                          #:id view-id
                          #:name "Test Read View"
                          #:metadata '(("author" . "tester"))
                          #:content '(("550e8400-e29b-41d4-a716-446655440101" . "block1")))))
    
    ;; Clean up before test
    (cleanup-test-storage "target/test-storage-read-view")
    
    ;; Save view first
    (save original-view storage)
    
    ;; Read back the view
    (let ((read-view (read-from view-id storage)))
      (test-assert "read-from returns a view object" (is-a? read-view <view>))
      (test-equal "read view has correct id" view-id (get-id read-view))
      (test-equal "read view has correct name" "Test Read View" (get-name read-view))
      (test-equal "read view has correct metadata" 
                  '(("author" . "tester")) 
                  (get-metadata read-view))
      (test-equal "read view has correct content" 
                  '(("550e8400-e29b-41d4-a716-446655440101" . "block1")) 
                  (get-content read-view)))
    
    ;; Clean up after test
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
    
    ;; Clean up before test
    (cleanup-test-storage "target/test-storage-read-block")
    
    ;; Save block first
    (save original-block storage)
    
    ;; Read back the block
    (let ((read-block (read-from block-id storage)))
      (test-assert "read-from returns a block object" (is-a? read-block <block>))
      (test-equal "read block has correct id" block-id (get-id read-block))
      (test-equal "read block has correct description" 
                  "Test Read Block" 
                  (get-description read-block))
      (test-equal "read block has correct source" 
                  "test source content" 
                  (get-source read-block))
      (test-equal "read block has correct tags" 
                  '("tag1" "tag2") 
                  (get-tags read-block))
      (test-equal "read block has correct created date" 
                  "2024-01-01" 
                  (get-created read-block))
      (test-equal "read block has correct modified date" 
                  "2024-01-02" 
                  (get-modified read-block))
      (test-equal "read block has correct metadata" 
                  '(("key" . "value")) 
                  (get-metadata read-block)))
    
    ;; Clean up after test
    (cleanup-test-storage "target/test-storage-read-block")))

;; Test read-from with text block
(test-group "read-from text block"
  (let* ((storage (make <filest> #:path "target/test-storage-read-text"))
         (text-id "550e8400-e29b-41d4-a716-446655440120")
         (original-text (make <text>
                          #:id text-id
                          #:description "Test Read Text"
                          #:source "text content here"
                          #:tags '("text" "test")
                          #:created "2024-01-01"
                          #:modified "2024-01-02")))
    
    ;; Clean up before test
    (cleanup-test-storage "target/test-storage-read-text")
    
    ;; Save text block first
    (save original-text storage)
    
    ;; Read back the text block
    (let ((read-text (read-from text-id storage)))
      (test-assert "read-from returns a text object" (is-a? read-text <text>))
      (test-equal "read text has correct id" text-id (get-id read-text))
      (test-equal "read text has correct description" 
                  "Test Read Text" 
                  (get-description read-text))
      (test-equal "read text has correct source" 
                  "text content here" 
                  (get-source read-text))
      (test-equal "read text has correct type" "text" (get-type read-text)))
    
    ;; Clean up after test
    (cleanup-test-storage "target/test-storage-read-text")))

;; Test read-from with non-existent id
(test-group "read-from non-existent id"
  (let* ((storage (make <filest> #:path "target/test-storage-read-notfound"))
         (non-existent-id "550e8400-e29b-41d4-a716-999999999999"))
    
    ;; Clean up before test
    (cleanup-test-storage "target/test-storage-read-notfound")
    
    ;; Try to read non-existent id
    (let ((result (read-from non-existent-id storage)))
      (test-assert "read-from returns #f for non-existent id" (not result)))
    
    ;; Clean up after test
    (cleanup-test-storage "target/test-storage-read-notfound")))

;; Test read-from searches view directory first
(test-group "read-from searches view directory first"
  (let* ((storage (make <filest> #:path "target/test-storage-read-priority"))
         (same-id "550e8400-e29b-41d4-a716-446655440130")
         (view (make <view>
                 #:id same-id
                 #:name "View with same ID"))
         (block (make <block>
                  #:id same-id
                  #:description "Block with same ID"
                  #:source "content"
                  #:tags '("test")
                  #:created "2024-01-01"
                  #:modified "2024-01-01")))
    
    ;; Clean up before test
    (cleanup-test-storage "target/test-storage-read-priority")
    
    ;; Save both view and block with same ID
    (save view storage)
    (save block storage)
    
    ;; Read-from should return the view (searched first)
    (let ((result (read-from same-id storage)))
      (test-assert "read-from returns view when both exist" (is-a? result <view>))
      (test-equal "returned object is the view" "View with same ID" (get-name result)))
    
    ;; Clean up after test
    (cleanup-test-storage "target/test-storage-read-priority")))

;; Test round-trip: save and read-from
(test-group "round-trip save and read-from"
  (let* ((storage (make <filest> #:path "target/test-storage-roundtrip"))
         (view-id "550e8400-e29b-41d4-a716-446655440140")
         (block-id "550e8400-e29b-41d4-a716-446655440141")
         (original-view (make <view>
                          #:id view-id
                          #:name "Roundtrip View"
                          #:metadata '(("key1" . "value1") ("key2" . "value2"))
                          #:content '(("550e8400-e29b-41d4-a716-446655440142" . "b1") 
                                       ("550e8400-e29b-41d4-a716-446655440143" . "b2"))))
         (original-block (make <block>
                           #:id block-id
                           #:description "Roundtrip Block"
                           #:source "roundtrip source"
                           #:tags '("round" "trip")
                           #:created "2024-01-01"
                           #:modified "2024-01-02"
                           #:metadata '(("meta1" . "val1")))))
    
    ;; Clean up before test
    (cleanup-test-storage "target/test-storage-roundtrip")
    
    ;; Save both objects
    (save original-view storage)
    (save original-block storage)
    
    ;; Read them back
    (let ((read-view (read-from view-id storage))
          (read-block (read-from block-id storage)))
      
      ;; Verify view round-trip
      (test-equal "view id matches after round-trip" 
                  (get-id original-view) 
                  (get-id read-view))
      (test-equal "view name matches after round-trip" 
                  (get-name original-view) 
                  (get-name read-view))
      (test-equal "view metadata matches after round-trip" 
                  (get-metadata original-view) 
                  (get-metadata read-view))
      (test-equal "view content matches after round-trip" 
                  (get-content original-view) 
                  (get-content read-view))
      
      ;; Verify block round-trip
      (test-equal "block id matches after round-trip" 
                  (get-id original-block) 
                  (get-id read-block))
      (test-equal "block description matches after round-trip" 
                  (get-description original-block) 
                  (get-description read-block))
      (test-equal "block source matches after round-trip" 
                  (get-source original-block) 
                  (get-source read-block))
      (test-equal "block tags match after round-trip" 
                  (get-tags original-block) 
                  (get-tags read-block))
      (test-equal "block metadata matches after round-trip" 
                  (get-metadata original-block) 
                  (get-metadata read-block)))
    
    ;; Clean up after test
    (cleanup-test-storage "target/test-storage-roundtrip")))

;; Test read-from with empty storage
(test-group "read-from with empty storage"
  (let* ((storage (make <filest> #:path "target/test-storage-empty"))
         (any-id "550e8400-e29b-41d4-a716-446655440150"))
    
    ;; Clean up before test
    (cleanup-test-storage "target/test-storage-empty")
    
    ;; Try to read from empty storage
    (let ((result (read-from any-id storage)))
      (test-assert "read-from returns #f from empty storage" (not result)))
    
    ;; Clean up after test
    (cleanup-test-storage "target/test-storage-empty")))

;; Test read-from with multiple blocks
(test-group "read-from with multiple blocks"
  (let* ((storage (make <filest> #:path "target/test-storage-multi-blocks"))
         (block-id-1 "550e8400-e29b-41d4-a716-446655440160")
         (block-id-2 "550e8400-e29b-41d4-a716-446655440161")
         (block-id-3 "550e8400-e29b-41d4-a716-446655440162")
         (block1 (make <block>
                   #:id block-id-1
                   #:description "Block 1"
                   #:source "source 1"
                   #:tags '("b1")
                   #:created "2024-01-01"
                   #:modified "2024-01-01"))
         (block2 (make <block>
                   #:id block-id-2
                   #:description "Block 2"
                   #:source "source 2"
                   #:tags '("b2")
                   #:created "2024-01-01"
                   #:modified "2024-01-01"))
         (block3 (make <block>
                   #:id block-id-3
                   #:description "Block 3"
                   #:source "source 3"
                   #:tags '("b3")
                   #:created "2024-01-01"
                   #:modified "2024-01-01")))
    
    ;; Clean up before test
    (cleanup-test-storage "target/test-storage-multi-blocks")
    
    ;; Save all blocks
    (save block1 storage)
    (save block2 storage)
    (save block3 storage)
    
    ;; Read each block individually
    (let ((read-block1 (read-from block-id-1 storage))
          (read-block2 (read-from block-id-2 storage))
          (read-block3 (read-from block-id-3 storage)))
      
      (test-equal "read block 1 description" "Block 1" (get-description read-block1))
      (test-equal "read block 2 description" "Block 2" (get-description read-block2))
      (test-equal "read block 3 description" "Block 3" (get-description read-block3)))
    
    ;; Clean up after test
    (cleanup-test-storage "target/test-storage-multi-blocks")))

(test-end "logs/storage-tests")
