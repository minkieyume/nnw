(use-modules (oop goops)
	     (nnw core generic)
	     (nnw core view)
	     (nnw core view document)
	     (nnw core block)	     
	     (nnw core parser)
	     (sxml match)
	     (srfi srfi-1)
             (srfi srfi-64))

(test-begin "logs/nnwio")

;; (test-group "parse a document"
;;   (let* ((view-source "This is a document source in Block 1.\nThis is the second line in Block 2")
;; 	 (parsed (parse view-source <document> '())))
;;     (test-assert "parsed returns a pair" (pair? parsed))
;;     (test-assert "parsed has 2 blocks" (= (length (cdr parsed)) 2))
;;     (test-assert "car parsed is document" (is-a? (car parsed) <view>))
;;     (test-assert "cdr parsed all is block" (every (lambda (b) (is-a? b <block>)) (cdr parsed)))))

;; TODO 将下面的单元测试改为sxml match匹配。
(test-group "parse-text basic functionality"
  (let ((result (parse-text "line1\nline2\n\nline3")))
    (test-equal "should create view with correct structure"
                'view
                (car result))
    
    (test-equal "should filter empty lines and create 3 blocks"
                3
                (length (cddr result)))
    
    (test-equal "should set default view type"
                "document"
                (cadr (assoc 'type (cadr result))))
    
    (test-equal "should set default view name"
                "Untitled Document"
                (cadr (assoc 'name (cadr result))))
    
    (test-equal "first block should contain line1"
                "line1"
                (caddr (caddr result)))
    
    (test-equal "blocks should have text type"
                "text"
                (cadr (assoc 'type (cadr (caddr result)))))))

(test-end "logs/nnwio")
