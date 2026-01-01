(use-modules (oop goops)
	     (nnw core view)
	     (nnw core utils)
             (srfi srfi-64))

(test-begin "logs/view-tests")

;; Test successful view creation with all parameters
(test-group "make-view success cases"
  (let ((view (make <view>
                #:name "Test View"
                #:metadata '(("key1" . "value1") ("key2" . "value2"))
                #:content '(("550e8400-e29b-41d4-a716-446655440000" . "block1")
                           ("550e8400-e29b-41d4-a716-446655440001" . "block2")
                           ("550e8400-e29b-41d4-a716-446655440002" . "block3")))))
    
    (test-assert "view is created" (is-a? view <view>))
    (test-equal "name is set" "Test View" (view-name view))
    (test-equal "metadata is set" '(("key1" . "value1") ("key2" . "value2")) (view-metadata view))
    (test-equal "content is set" '(("550e8400-e29b-41d4-a716-446655440000" . "block1")
                                   ("550e8400-e29b-41d4-a716-446655440001" . "block2")
                                   ("550e8400-e29b-41d4-a716-446655440002" . "block3")) (view-content view))
    (test-assert "id is generated" (string? (view-id view)))
    (test-assert "id is non-empty" (> (string-length (view-id view)) 0))))

;; Test view creation with minimal parameters
(test-group "make-view with minimal parameters"
  (let ((view (make <view> #:name "Minimal View")))
    
    (test-assert "view is created" (is-a? view <view>))
    (test-equal "name is set" "Minimal View" (view-name view))
    (test-equal "metadata defaults to empty list" '() (view-metadata view))
    (test-equal "content defaults to empty list" '() (view-content view))
    (test-assert "id is auto-generated" (string? (view-id view)))))

;; Test view creation with custom id
(test-group "make-view with custom id"
  (let ((custom-id "550e8400-e29b-41d4-a716-446655440000")
        (view (make <view>
                #:id "550e8400-e29b-41d4-a716-446655440000"
                #:name "Custom ID View")))
    
    (test-assert "view is created" (is-a? view <view>))
    (test-equal "custom id is set" custom-id (view-id view))
    (test-equal "name is set" "Custom ID View" (view-name view))))

;; Test ID generation uniqueness
(test-group "ID generation uniqueness"
  (let* ((view1 (make <view> #:name "View 1"))
         (view2 (make <view> #:name "View 2")))
    
    (test-assert "different views produce different IDs"
                 (not (string=? (view-id view1) (view-id view2))))))

;; Test view->string method
(test-group "view->string method"
  (let ((view (make <view>
                #:name "String Test View"
                #:metadata '(("author" . "test"))
                #:content '(("550e8400-e29b-41d4-a716-446655440000" . "block1")))))
    
    (test-equal "view->string returns view name"
                "String Test View"
                (view->string view))))

;; Test view->string with different names
(test-group "view->string with various names"
  (let ((view1 (make <view> #:name "Simple"))
        (view2 (make <view> #:name "View with spaces"))
        (view3 (make <view> #:name "中文视图名称")))
    
    (test-equal "simple name" "Simple" (view->string view1))
    (test-equal "name with spaces" "View with spaces" (view->string view2))
    (test-equal "Chinese name" "中文视图名称" (view->string view3))))

;; Test empty metadata and content
(test-group "empty metadata and content"
  (let ((view (make <view>
                #:name "Empty View"
                #:metadata '()
                #:content '())))
    
    (test-assert "view is created with empty metadata and content" (is-a? view <view>))
    (test-equal "metadata is empty" '() (view-metadata view))
    (test-equal "content is empty" '() (view-content view))))

;; Test metadata with various structures
(test-group "metadata with various structures"
  (let ((view (make <view>
                #:name "Complex Metadata View"
                #:metadata '(("string" . "value")
                            ("number" . 42)
                            ("list" . (1 2 3))
                            ("nested" . (("inner" . "data")))))))
    
    (test-assert "view is created with complex metadata" (is-a? view <view>))
    (test-equal "complex metadata is preserved"
                '(("string" . "value")
                  ("number" . 42)
                  ("list" . (1 2 3))
                  ("nested" . (("inner" . "data"))))
                (view-metadata view))))

;; Test content with various block references
(test-group "content with various block references"
  (let ((view (make <view>
                #:name "Content View"
                #:content '(("550e8400-e29b-41d4-a716-446655440000" . "data1")
                           ("550e8400-e29b-41d4-a716-446655440001" . "data2")
                           ("550e8400-e29b-41d4-a716-446655440002" . "data3")
                           ("550e8400-e29b-41d4-a716-446655440003" . "data4")))))
    
    (test-assert "view is created with content" (is-a? view <view>))
    (test-equal "content is preserved" '(("550e8400-e29b-41d4-a716-446655440000" . "data1")
                                         ("550e8400-e29b-41d4-a716-446655440001" . "data2")
                                         ("550e8400-e29b-41d4-a716-446655440002" . "data3")
                                         ("550e8400-e29b-41d4-a716-446655440003" . "data4")) (view-content view))))

;; Test UUID v4 format validation
(test-group "uuid-v4-validation"
  (test-assert "valid UUID v4 string is recognized"
               (uuid-v4-string? "550e8400-e29b-41d4-a716-446655440000"))
  (test-assert "invalid UUID format is rejected"
               (not (uuid-v4-string? "not-a-uuid")))
  (test-assert "non-string is rejected"
               (not (uuid-v4-string? 12345)))
  (test-assert "UUID v1 format is rejected"
               (not (uuid-v4-string? "550e8400-e29b-11d4-a716-446655440000")))
  (test-assert "UUID with wrong version is rejected"
               (not (uuid-v4-string? "550e8400-e29b-21d4-a716-446655440000"))))

;; Test valid-content? function
(test-group "valid-content-validation"
  (test-assert "empty content is valid"
               (valid-content? '()))
  (test-assert "content with valid UUID v4 keys is valid"
               (valid-content? '(("550e8400-e29b-41d4-a716-446655440000" . "data1")
                                ("550e8400-e29b-41d4-a716-446655440001" . "data2"))))
  (test-assert "content with invalid UUID keys is rejected"
               (not (valid-content? '(("invalid-uuid" . "data1")))))
  (test-assert "non-list content is rejected"
               (not (valid-content? "not-a-list")))
  (test-assert "content with non-pair items is rejected"
               (not (valid-content? '("550e8400-e29b-41d4-a716-446655440000")))))

;; Test initialize method type checking
(test-group "initialize-type-checking"
  (test-error "missing name raises error"
              (make <view>))
  (test-error "non-string name raises error"
              (make <view> #:name 123))
  (test-error "non-list metadata raises error"
              (make <view> #:name "Test" #:metadata "not-a-list"))
  (test-error "non-list content raises error"
              (make <view> #:name "Test" #:content "not-a-list"))
  (test-error "content with invalid UUID keys raises error"
              (make <view> 
                #:name "Test"
                #:content '(("invalid-uuid" . "data"))))
  (test-assert "valid view creation succeeds"
               (is-a? (make <view>
                        #:name "Valid View"
                        #:metadata '(("key" . "value"))
                        #:content '(("550e8400-e29b-41d4-a716-446655440000" . "data")))
                      <view>)))

(test-end "logs/view-tests")
