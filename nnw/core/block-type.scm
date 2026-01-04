(define-module (nnw core type)
  #:use-module (nnw core generic)
  #:use-module (nnw core block)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:export (symbol->block-type
	    string->block-type))

(define (symbol->block-type symb)
  (match symb
    ('block <block>)
    ('text <text>)))

(define (string->block-type str)
  (symbol->block-type (string->symbol str)))
