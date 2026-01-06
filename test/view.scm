(use-modules (oop goops)
	     (nnw core generic)
	     (nnw core view)
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

(test-end "logs/view-tests")
