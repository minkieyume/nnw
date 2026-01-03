(define-module (nnw core storage)
  #:use-module (nnw core generic)
  #:use-module (nnw core serilize)
  #:use-module (nnw core view)
  #:use-module (nnw core block)
  #:use-module (oop goops)
  #:use-module (ice-9 textual-ports)
  #:export (<storage>
	    <filest>
	    get-path
	    read-from))

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

;; Read and deserialize object from file storage by ID
(define-method (read-from (id <string>) (storage <filest>))
  (let* ((base-path (get-path storage))
         (view-path (string-append base-path "/view/" id ".scm"))
         (block-path (string-append base-path "/block/" id ".scm")))
    (cond
     ;; Try to read from view directory first
     ((file-exists? view-path)
      (call-with-input-file view-path
        (lambda (port)
          (let ((data (read port)))
            (unserilize data)))))
     ;; Then try block directory
     ((file-exists? block-path)
      (call-with-input-file block-path
        (lambda (port)
          (let ((data (read port)))
            (unserilize data)))))
     ;; If neither exists, return #f
     (else #f))))
