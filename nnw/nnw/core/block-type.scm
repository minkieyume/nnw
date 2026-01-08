(define-module (nnw core block-type)
  #:use-module (nnw core generic)
  #:use-module (nnw core block)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:use-module (sxml match)
  #:export (symbol->block-type
	    string->block-type))

(define (symbol->block-type symb)
  (match symb
    ('block <block>)
    ('text <text>)))

(define (string->block-type str)
  (symbol->block-type (string->symbol str)))

(define-method (input->block (input-block <list>))
  (sxml-match-let (((block (@ (type ,type) . ,oth) . ,chd) input-block))
     (input->block input-block (string->block-type type))))
