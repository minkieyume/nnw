(define-module (nnw core storage)
  #:use-module (nnw core generic)
  #:use-module (nnw core utils)
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

(define-method (save (storable <storable>) (storage <filest>))
  (let* ((base-path (get-path storage))
         (dir (string-append base-path
			     (cond ((is-a? storable <view>) "/view")
				   ((is-a? storable <block>) "/block"))))
	 (file-name (string-append (get-id storable) ".scm")))
    ;; Serialize and write to file
    (save-to-dir dir file-name (serilize storable))))

(define-method (read-from-view-path view-path)
  (call-with-input-file view-path
    (lambda (port)
      (let ((data (read port)))
        (unserilize data)))))

(define-method (read-from-block-path block-path)
  (call-with-input-file block-path
    (lambda (port)
      (let ((data (read port)))
        (unserilize data)))))

;; Read and deserialize object from file storage by ID
(define-method (read-from (id <string>) (storage <filest>))
  (let* ((base-path (get-path storage))
         (view-path (string-append base-path "/view/" id ".scm"))
         (block-path (string-append base-path "/block/" id ".scm")))
    (cond
     ;; Try to read from view directory first
     ((file-exists? view-path)
      (read-from-view-path view-path))
     ;; Then try block directory
     ((file-exists? block-path)
      (read-from-block-path block-path))
     ;; If neither exists, return #f
     (else #f))))
