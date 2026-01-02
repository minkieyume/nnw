(define-module (nnw core parser)
  #:use-module (nnw core block)
  #:use-module (nnw core view)
  #:use-module (nnw core utils)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (parse))

;; Get current timestamp in ISO 8601 format
(define (current-timestamp)
  (date->string (current-date) "~Y-~m-~dT~H:~M:~S"))

;; Parse document source into blocks
;; Simple parser: split by newline
(define (parse-document-source source)
  (let ((lines (string-split source #\newline)))
    (filter (lambda (s) (not (string=? s ""))) lines)))

;; Parse source into a view and its blocks
;; Returns a pair: (view . blocks)
(define* (parse source view-class
                 #:key
                 (tags '())
                 (view-id #f)
                 (view-name "Untitled Document")
                 (view-metadata '()))
  "Parse document source into a view and its blocks.
   Returns a pair (view . blocks) where view is an instance of view-class
   and blocks is a list of <block> instances."
  
  (unless (string? source)
    (error "source must be a string" source))
  
  (unless (list-of-string? tags)
    (error "tags must be a list of strings" tags))
  
  (when view-id
    (unless (uuid-v4-string? view-id)
      (error "view-id must be a valid UUID v4 string" view-id)))
  
  (let* ((block-sources (parse-document-source source))
         (timestamp (current-timestamp))
         (blocks '())
         (content '()))
    
    ;; Create blocks for each source line
    (let loop ((sources block-sources)
               (index 0))
      (unless (null? sources)
        (let* ((block-source (car sources))
               (block (make view-class
                        #:description (string-append "Block " (number->string (+ index 1)))
                        #:source block-source
                        #:type "text"
                        #:tags tags
                        #:created timestamp
                        #:modified timestamp)))
          (set! blocks (cons block blocks))
          (set! content (cons (cons (block-id block) index) content))
          (loop (cdr sources) (+ index 1)))))
    
    ;; Create view instance
    (let ((view (make view-class
                  #:id view-id
                  #:name view-name
                  #:metadata view-metadata
                  #:content (reverse content))))
      
      ;; Return pair of (view . blocks)
      (cons view (reverse blocks)))))
