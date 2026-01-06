(define-module (nnw core writer)
  #:use-module (nnw core generic)
  #:use-module (nnw core utils)
  #:use-module (nnw core view)
  #:use-module (oop goops)
  #:use-module (sxml match)
  #:export (write->text))

(define-generic write->text)

(define-method (write->text (sxml <list>))
  (sxml-match sxml
    ((view (@ . ,attrs) . ,children)
     (string-join
      (filter-map (lambda (child)
                    (sxml-match child
                      ((ref (@ (id ,id)) ,content)
                       content)
                      (,otherwise #f)))
                  children)
      "\n"))
    (,otherwise "")))
