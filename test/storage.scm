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

(test-end "logs/storage-tests")
