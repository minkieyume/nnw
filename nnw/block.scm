(define-module (nnw block)
  #:use-module (oop goops)
  #:use-module (uuid generate)
  #:export (<block>))

(define-class <block> ()
  (id #:init-thunk generate-string-uuid)
  descrption
  source
  type
  tags
  created
  modified)
