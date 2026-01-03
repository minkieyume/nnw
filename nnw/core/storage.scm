(define-module (nnw core storage)
  #:use-module (nnw core generic)
  #:use-module (oop goops)
  #:use-module (ice-9 textual-ports)
  #:export (*view-storage*
	    *block-storage*
	    <storage>
	    <filest>
	    get-path))

;; Global storage for views and blocks (in-memory for now)
(define *view-storage* (make-hash-table))
(define *block-storage* (make-hash-table))

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
