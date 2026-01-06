(use-modules (oop goops)
	     (nnw core generic)
	     (nnw core view)
	     (nnw core view document)
	     (nnw core block)
	     (nnw core writer)
	     (nnw core input)
	     (sxml match)
	     (srfi srfi-1)
             (srfi srfi-64))

(test-begin "logs/writer")

;; TODO 帮我改写这个测试，改成手动构造一个只带有block-id的view的sxml表达式，并手动调用storage的save存储手动构造的对应id的block，以便测试。测试完成后删掉文件。
(test-group "write a view to text, get same value"
  (let* ((views-blocks (nnw-input "This is a document source in Block 1.\nThis is the second line in Block 2"))
	 (view (caar views-blocks)))
    (test-equal "can write a output->text" "This is a document source in Block 1.\nThis is the second line in Block 2"
		(write->text (view->output view)))))

(test-end "logs/writer")
