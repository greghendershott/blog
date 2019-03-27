#lang racket/base

(require racket/require
         (multi-in racket (match path))
         (multi-in web-server (dispatchers/dispatch servlet-env)))

(module+ main (preview))

(define (preview)
  (match (current-command-line-arguments)
    [(vector index.html)
     (serve/servlet (lambda (_) (next-dispatcher))
                    #:servlet-path      "/index.html"
                    #:extra-files-paths (list (or (path-only index.html)
                                                  (current-directory)))
                    #:listen-ip         "127.0.0.1"
                    #:port              3000
                    #:launch-browser?   #t)]))
