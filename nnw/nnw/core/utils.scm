(define-module (nnw core utils)
  #:use-module (nnw core generic)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-19)
  #:use-module (gcrypt hash)
  #:use-module (rnrs bytevectors)
  #:export (list-of-string?
	    uuid-v4-string?
	    current-timestamp
	    generate-hash
	    save-to-dir
	    fold-2lst-pairs))

(define (list-of-string? lst)
  (and (list? lst)
       (every string? lst)))

;; Check if a string is a valid UUID v4 format
(define (uuid-v4-string? str)
  (and (string? str)
       (let ((pattern "^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$"))
         (string-match pattern str))))

;; Get current timestamp in ISO 8601 format
(define (current-timestamp)
  (date->string (current-date) "~Y-~m-~dT~H:~M:~S"))

;; Convert strings to hash with split.
(define (generate-hash split . strings)
  (let* ((combind-str (string-join strings split))
         (hash-bytes (sha256 (string->utf8 combind-str))))    
    ;; Convert bytevector to hexadecimal string representation
    (string-join
     (map (lambda (byte)
            (format #f "~2,'0x" byte))
          (bytevector->u8-list hash-bytes))
     "")))

(define (save-to-dir dir name context)
  ;; Create directory if it doesn't exist
  (system* "mkdir" "-p" dir)
  
  (call-with-output-file (string-append dir "/" name)
    (lambda (port)
      (write context port)
      (newline port))))

(define (fold-2lst-pairs . pairs)
  (fold (lambda (pair acc)
	  (cons (append (car pair) (car acc))
		(append (cdr pair) (cdr acc)))) '(() . ()) pairs))
