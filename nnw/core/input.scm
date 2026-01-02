(define-module (nnw core input)
  #:use-module (nnw core block)
  #:use-module ((nnw core view) #:prefix nnw:)
  #:use-module (nnw core view document)
  #:use-module (nnw core utils)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (nnw-input
            *view-storage*
            *block-storage*))

;; Global storage for views and blocks (in-memory for now)
(define *view-storage* (make-hash-table))
(define *block-storage* (make-hash-table))

;; Get current timestamp in ISO 8601 format
(define (current-timestamp)
  (date->string (current-date) "~Y-~m-~dT~H:~M:~S"))

;; Parse document source into blocks
;; Simple parser: split by newline
(define (parse-document-source source)
  (let ((lines (string-split source #\newline)))
    (filter (lambda (s) (not (string=? s ""))) lines)))

;; Input entry point
(define* (nnw-input source 
                    #:key 
                    (tags '())
                    (view-id #f)
                    (view-type "document")
                    (view-name "Untitled Document")
                    (view-metadata '()))
  "Parse and store a document view with its blocks"
  
  (unless (string? source)
    (error "source must be a string" source))
  
  (unless (list-of-string? tags)
    (error "tags must be a list of strings" tags))
  
  (when view-id
    (unless (uuid-v4-string? view-id)
      (error "view-id must be a valid UUID v4 string" view-id)))
  
  ;; TODO 重构下面代码，将解析相关代码实现到 'parser.scm' 中，实现为 `parse` 方法，并确保通过test/parser.scm的测试。
  (let* ((block-sources (parse-document-source source))
         (timestamp (current-timestamp))
         (blocks '())
         (content '()))
    
    ;; Create blocks for each source line
    (let loop ((sources block-sources)
               (index 0))
      (unless (null? sources)
        (let* ((block-source (car sources))
               (block (make <block>
                        #:description (string-append "Block " (number->string (+ index 1)))
                        #:source block-source
                        #:type "text"
                        #:tags tags
                        #:created timestamp
                        #:modified timestamp)))
          ;; Store block
          (hash-set! *block-storage* (block-id block) block)
          (set! blocks (cons block blocks))
          (set! content (cons (cons (block-id block) index) content))
          (loop (cdr sources) (+ index 1)))))
    
    ;; Create document view
    (let ((doc (make <document>
                 #:id view-id
                 #:name view-name
                 #:metadata view-metadata
                 #:content (reverse content))))
      
      ;; Store view
      (hash-set! *view-storage* (nnw:view-id doc) doc)
      
      ;; Return view id
      (nnw:view-id doc))))
