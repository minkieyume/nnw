(define-module (nnw core storage)
  #:use-module (nnw core generic)
  #:export (*view-storage*
	    *block-storage*))

;; Global storage for views and blocks (in-memory for now)
(define *view-storage* (make-hash-table))
(define *block-storage* (make-hash-table))

;; TODO 实现storage类及save方法，同时为 'block.scm' 和 'view.scm' 中的 block 和view实现serilize和unserilize方法，具体实现要求可参考 'block.scm' 的单元测试的最后一个group。

(define-class <storage> ())

(define-class <filest> (<storage>)
  (path "target/storage")) ;; view in path/to/view/ block in path/to/block/

(define-method (save (view <view>) (storage <storage>)))

(define-method (save (view <block>) (storage <storage>)))
