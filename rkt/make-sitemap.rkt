#lang racket/base

(require racket/require
         (multi-in racket (contract file format match path string))
         "site.rkt"
         "tag-posts.rkt"
         "util.rkt")

(module+ main (main))

(define (main)
  (match (current-command-line-arguments)
    [(vector sitemap.txt) (write-sitemap sitemap.txt)]))

(define (write-sitemap sitemap.txt)
  (call-with-output-file*/delete
   #:exists 'replace sitemap.txt
   (λ (out)
     (for ([path (in-list (directory-list (path-only sitemap.txt)))]
           #:when (and (equal? #".html" (path-get-extension path))
                       (not (directory-exists? path))))
       (displayln (full-uri (path->string path))
                  out)))))
