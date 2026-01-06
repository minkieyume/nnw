(use-modules (oop goops)
	     (nnw core generic)
	     (nnw core view)
	     (nnw core utils)
	     (nnw core input)
	     (nnw core output)
             (srfi srfi-64))

(test-begin "logs/nnwio")

(test-group "input a document and get output"
  (let ((view-source "This is a document source in Block 1.\nThis is the second line in Block 2"))
    (nnw-input view-source
	       #:tags '("document" "test")
	       #:view-id "550e8400-e29b-41d4-a716-446655440002"
	       #:view-type "document")
    (test-equal "same with input and output" view-source (nnw-output "550e8400-e29b-41d4-a716-446655440002"))))

(test-end "logs/nnwio")
