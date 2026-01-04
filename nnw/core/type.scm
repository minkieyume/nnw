(define-module (nnw core type)
  #:use-module (nnw core generic)
  #:use-module (nnw core block)
  #:use-module (nnw core view)
  #:use-module (nnw core view document)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:export (symbol->view-type
	    symbol->block-type
	    string->view-type
	    string->block-type))

(define (symbol->view-type symb)
  (match symb
    ('view <view>)
    ('document <document>)))

(define (symbol->block-type symb)
  (match symb
    ('block <block>)
    ('text <text>)))

(define (string->view-type str)
  (symbol->view-type (string->symbol str)))

(define (string->block-type str)
  (symbol->block-type (string->symbol str)))
