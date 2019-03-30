#lang racket/base

(require racket/contract)

(provide call-with-output-file*/delete
         sans-top-dir)

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

(define/contract (sans-top-dir p)
  (-> (and/c path-string? relative-path?)
      (and/c path-string? relative-path? string?))
  (path->string (apply build-path (cdr (explode-path p)))))

(module+ test
  (require rackunit)
  (check-equal? (sans-top-dir "www/path/to/foo.html")
                "path/to/foo.html")
  (check-equal? (sans-top-dir (build-path "www" "path" "to" "foo.html"))
                "path/to/foo.html"))
