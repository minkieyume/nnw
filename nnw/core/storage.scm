(define-module (nnw core storage)
  #:use-module (nnw core generic)
  #:use-module (nnw core serilize)
  #:use-module (nnw core view)
  #:use-module (nnw core block)
  #:use-module (oop goops)
  #:use-module (ice-9 textual-ports)
  #:export (<storage>
	    <filest>
	    get-path))

(define-class <storage> ())

(define-class <filest> (<storage>)
  (path #:init-keyword #:path 
        #:init-value "target/storage"
        #:getter get-path))

;; Save view to file storage
(define-method (save (view <view>) (storage <filest>))
  (let* ((base-path (get-path storage))
         (view-dir (string-append base-path "/view"))
         (file-path (string-append view-dir "/" (get-id view) ".scm")))
    ;; Create directory if it doesn't exist
    (system* "mkdir" "-p" view-dir)
    ;; Serialize and write to file
    (call-with-output-file file-path
      (lambda (port)
        (write (serilize view) port)
        (newline port)))))

;; Save block to file storage
(define-method (save (block <block>) (storage <filest>))
  (let* ((base-path (get-path storage))
         (block-dir (string-append base-path "/block"))
         (file-path (string-append block-dir "/" (get-id block) ".scm")))
    ;; Create directory if it doesn't exist
    (system* "mkdir" "-p" block-dir)
    ;; Serialize and write to file
    (call-with-output-file file-path
      (lambda (port)
        (write (serilize block) port)
        (newline port)))))

;; TODO 更新read-from代码，确保其能正确通过搜索view和block的路径，实现反序列化并返回值。如果找不到对应id的文件返回#f。
(define-method (read-from (id <string>) (storage <filest>)))
