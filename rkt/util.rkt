#lang racket/base

(require racket/require
         (multi-in racket (contract format path)))

(provide call-with-output-file*/delete
         sans-top-dir
         file->uri)

(module+ test
  (require rackunit))

(define (call-with-output-file*/delete path
                                       proc
                                       #:mode [mode 'binary]
                                       #:exists [exists 'error])
  (with-handlers ([exn? (λ (e)
                          (when (file-exists? path)
                            (delete-file path))
                          (raise e))])
    (call-with-output-file*
      path
      #:mode mode
      #:exists exists
      proc)))

;; e.g. ".cache/something" => "something"
(define/contract (sans-top-dir p)
  (-> (and/c path-string? relative-path?)
      (and/c path-string? relative-path? string?))
  (path->string (apply build-path (cdr (explode-path p)))))

(module+ test
  (check-equal? (sans-top-dir ".cache/path/to/foo.html")
                "path/to/foo.html")
  (check-equal? (file->uri (build-path ".cache" "path" "to" "foo.html"))
                "path/to/foo.html"))

(define/contract (file->uri www-root file-path)
  (-> path-string? path-string?
      (and/c path-string? relative-path? string?))
  (path->string (find-relative-path www-root file-path)))

(module+ test
  ;; Where relative
  (check-equal? (file->uri "www"
                           "www/path/to/foo.html")
                "path/to/foo.html")
  (check-equal? (file->uri (build-path "www")
                           (build-path "www" "path" "to" "foo.html"))
                "path/to/foo.html")
  ;; Where absolute
  (check-equal? (file->uri "/home/greg/src/www"
                           "/home/greg/src/www/path/to/foo.html")
                "path/to/foo.html")
  (check-equal? (file->uri (build-path "/home" "greg" "src" "www")
                           (build-path "/home" "greg" "src" "www" "path" "to" "foo.html"))
                "path/to/foo.html"))
