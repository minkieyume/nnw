(define-module (nnw core view)
  #:use-module (oop goops)
  #:use-module (uuid generate)
  #:export (<view>
	    view-id
	    view-name
	    view-metadata
	    view-content
	    view->string))

(define-class <view> ()
  (id #:init-keyword #:id
      #:init-thunk generate-string-uuid
      #:getter view-id)
  (name #:init-keyword #:name #:getter view-name)
  (metadata #:init-keyword #:metadata #:init-value '() #:getter view-metadata)
  (content #:init-keyword #:content #:init-value '() #:getter view-content))

;; Format a view to a string
(define-method (view->string (view <view>))
  (view-name view))

;; TODO 为content实现独立的类型检查函数，要求content为一个alist，且content的键必须为符合v4格式的uuid string。

;; TODO 给initialize方法添加类型检查，并修改与之相关的单元测试，避免其它单元测试报错，同时还要添加针对initialize的类型检查是否生效的单元测试。
(define-method (initialize (view <view>) . initargs)
  (next-method))
