(define-module (nnw core type)
  #:use-module (nnw core view-type)
  #:use-module (nnw core block-type)
  #:re-export (symbol->view-type
	       string->view-type
	       symbol->block-type
	       string->block-type))
