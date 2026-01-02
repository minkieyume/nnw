(define-module (nnw core view document)
  #:use-module (nnw core view)
  #:use-module (oop goops)
  #:use-module (uuid generate)
  #:export (<document>))

;; TODO 实现document视图，document视图只是一个简单的，content的值为排序索引数值的视图，其view->string方法输出的是按索引顺序排版的block或view文本，要实现这个，可能需要单独实现一个根据id判定元素是block还是view的方法。
(define-class <document> (<view>)
  (type #:init-value "document"))
