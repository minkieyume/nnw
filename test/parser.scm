(use-modules (oop goops)
	     (nnw core generic)
	     (nnw core view)
	     (nnw core view document)
	     (nnw core block)	     
	     (srfi srfi-1)
             (srfi srfi-64))

(test-begin "logs/nnwio")

(test-group "parse a document"
  (let* ((view-source "This is a document source in Block 1.\nThis is the second line in Block 2")
	 (parsed (parse view-source <document> '())))
    (test-assert "parsed returns a pair" (pair? parsed))
    (test-assert "parsed has 2 blocks" (= (length (cdr parsed)) 2))
    (test-assert "car parsed is document" (is-a? (car parsed) <view>))
    (test-assert "cdr parsed all is block" (every (lambda (b) (is-a? b <block>)) (cdr parsed)))))

(test-end "logs/nnwio")
