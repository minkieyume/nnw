(define-module (nnw core view-type)
  #:use-module (nnw core generic)
  #:use-module (nnw core block)
  #:use-module (nnw core view)
  #:use-module (nnw core view list-view)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:export (symbol->view-type
	    string->view-type))

(define (symbol->view-type symb)
  (match symb
    ('view <view>)
    ('list-view <list-view>)))

(define (string->view-type str)
  (symbol->view-type (string->symbol str)))
