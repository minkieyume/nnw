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
    (sxml-match result
      ((view (@ (type "document")
                (name "Untitled Document"))
             (block (@ (type "text")) (p ,line1))
             (block (@ (type "text")) (p ,line2))
             (block (@ (type "text")) (p ,line3)))
       (test-assert "sxml match successful" #t)
       (test-equal "first block content" "line1" line1)
       (test-equal "second block content" "line2" line2)
       (test-equal "third block content" "line3" line3))
      (else
       (test-assert "sxml match failed" #f)))))

(test-end "logs/nnwio")
