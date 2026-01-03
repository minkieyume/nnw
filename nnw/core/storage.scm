(define-module (nnw core storage)
  #:use-module (nnw core generic)
  #:export (*view-storage*
	    *block-storage*))

;; Global storage for views and blocks (in-memory for now)
(define *view-storage* (make-hash-table))
(define *block-storage* (make-hash-table))

;; TODO 将
