#lang racket/base

(require racket/require
         (multi-in racket (contract format path)))

(provide call-with-output-file*/delete
         sans-top-dir
         file->uri
         split-at*
         split-at*/list)

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
  (check-equal? (sans-top-dir (build-path ".cache" "path" "to" "foo.html"))
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

;; Like racket/list split-at, but when n is greater than length of xs,
;; returns empty list(s) instead of raising exn.
(define (split-at* xs n)
  (let loop ([xs xs] [n n] [pfx '()])
    (cond [(zero? n) (values (reverse pfx) xs)]
          [(pair? xs) (loop (cdr xs) (sub1 n) (cons (car xs) pfx))]
          [else (values (reverse pfx) xs)])))

(define (split-at*/list xs n)
    (call-with-values (λ () (split-at* xs n)) list))

(module+ test
  (check-equal? (split-at*/list (list) 0)
                (list (list) (list)))
  (check-equal? (split-at*/list (list 0 1 2 3 4) 2)
                (list (list 0 1) (list 2 3 4)))
  (check-equal? (split-at*/list (list 0 1 2 3 4) 10)
                (list (list 0 1 2 3 4) (list))))
