(define-module (nnw core type)
  #:use-module (nnw core generic)
  #:use-module (nnw core block)
  #:use-module (nnw core view)
  #:use-module (nnw core view document)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:export (symbol->view-type
	    symbol->block-type))

(define (symbol->view-type symb)
  (match symb
    ('view <view>)
    ('document <document>)))

(define (symbol->block-type symb)
  (match symb
    ('block <block>)
    ('text <text>)))
