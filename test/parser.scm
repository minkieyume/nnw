(use-modules (oop goops)
	     (nnw core view)
	     (nnw core view document)
	     (nnw core block)
	     (nnw core parser)
	     (srfi srfi-1)
             (srfi srfi-64))

(test-begin "logs/nnwio")

;; TODO Finish this test.
(test-group "parse a document"
  (let* ((view-source "This is a document source in Block 1.\nThis is the second line in Block 2")
	 (parsed (parse view-source <document>)))
    (test-assert "car parsed is document" (is-a? (car parsed) <view>))
    (test-assert "cdr parsed all is block" (every (lambda (b) (is-a? b <block>)) (cdr parsed)))))

(test-end "logs/nnwio")
