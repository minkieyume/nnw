(use-modules (oop goops)
	     (nnw core generic)
	     (nnw core view)
	     (nnw core view document)  ; 添加这行
	     (nnw core utils)
             (srfi srfi-64)
	     (sxml match))

(test-begin "logs/view-tests")

;; Test successful view creation with all parameters
(test-group "make-view success cases"
  (let ((view (make <view>
                #:name "Test View"
                #:metadata '(("key1" . "value1") ("key2" . "value2"))
                #:content '(("550e8400-e29b-41d4-a716-446655440000" . "block1")
                            ("550e8400-e29b-41d4-a716-446655440001" . "block2")))))
    
    (test-assert "view is created" (is-a? view <view>))
    (test-equal "name is set" "Test View" (get-name view))
    (test-assert "id is generated" (string? (get-id view)))))

;; Test type validation
(test-group "type validation"
  (test-error "missing name raises error"
              (make <view>))
  
  (test-error "non-string name raises error"
              (make <view> #:name 123))
  
  (test-error "content with invalid UUID keys raises error"
              (make <view> 
                #:name "Test"
                #:content '(("invalid-uuid" . "data")))))

;; Test view->string method
(test-group "view->output method"
  (let ((view1 (make <view>
		 #:id "550e8400-e29b-41d4-a716-446655440001"
		 #:name "Test View"
                 #:content '(("550e8400-e29b-41d4-a716-446655440000" . "block1")
                             ("550e8400-e29b-41d4-a716-446655440001" . "block2")))))
    (sxml-match-let* (((view (@ (id ,id) (type ,type) (name ,name) . ,metadata) . ,children) (view->output view1))
		      ((ref (@ (id ,b1id)) . ,b1ct) (car children)))
      (test-equal "same id" "550e8400-e29b-41d4-a716-446655440001" id)
      (test-equal "same name" "Test View" name)
      (test-equal "same block1 id" "550e8400-e29b-41d4-a716-446655440000" b1id)
      (test-equal "same block1 ctx" '("block1") b1ct))))

;; Test document view->output method
(test-group "document view->output method"
  (let ((doc1 (make <document>
                #:id "550e8400-e29b-41d4-a716-446655440010"
                #:name "Test Document"
                #:content '(("550e8400-e29b-41d4-a716-446655440011" . 1)
                            ("550e8400-e29b-41d4-a716-446655440012" . 0)))))
    (sxml-match-let (((view (@ (id ,id) (type ,type) (name ,name)) . ,children) (view->output doc1)))
      (test-equal "document id" "550e8400-e29b-41d4-a716-446655440010" id)
      (test-equal "document type" "view" type)
      (test-equal "document name" "Test Document" name)
      (test-equal "document has two children" 2 (length children))
      ;; 验证子元素按索引排序（index 0 在前，index 1 在后）
      (sxml-match-let* (((ref (@ (id ,first-id))) (car children))
                        ((ref (@ (id ,second-id))) (cadr children)))
        (test-equal "first child has index 0" "550e8400-e29b-41d4-a716-446655440012" first-id)
        (test-equal "second child has index 1" "550e8400-e29b-41d4-a716-446655440011" second-id))))

  (let ((empty-doc (make <document>
                     #:id "550e8400-e29b-41d4-a716-446655440013"
                     #:name "Empty Document"
                     #:content '())))
    (sxml-match-let (((view (@ (id ,id) (type ,type) (name ,name)) . ,children) (view->output empty-doc)))
      (test-equal "empty document id" "550e8400-e29b-41d4-a716-446655440013" id)
      (test-equal "empty document name" "Empty Document" name)
      (test-equal "empty document has no children" '() children))))


(test-end "logs/view-tests")
