#lang at-exp racket/base

(require racket/require
         (multi-in racket (contract file format match path string))
         (only-in "compile-post.rkt" datetime+tags->xexpr)
         "post.rkt"
         "render-page.rkt"
         "util.rkt"
         "xexpr.rkt")

(module+ main (main))

(define (main)
  (match (current-command-line-arguments)
    [(vector tag-caches-path output-html)
     (define tags (sort (for*/list ([path (in-directory tag-caches-path)]
                                    [name (in-value (file-name-from-path path))]
                                    [str  (in-value (path->string name))]
                                    #:when (not (equal? str "all")))
                          str)
                        string-ci<?))
     (make-directory* (path-only output-html))
     (call-with-output-file*/delete
      #:exists 'replace output-html
      (λ (out)
        (displayln "<!DOCTYPE html>" out)
        (displayln (xexpr->string
                    (tags-xexpr tags output-html))
                   out)))]))

(define/contract (tags-xexpr tags page-path)
  (-> (listof string?) path-string? xexpr/c)
  (define links
    (for/list ([tag (in-list tags)])
      `(li (a ([href ,(~a "/tags/" tag ".html")]) ,tag))))
  (define contents
    `((ul ,@links)))
  (page-xexpr #:title       "Tags"
              #:keywords    (string-join tags ",")
              #:description "Tags"
              #:page-path   page-path
              #:atom-path   (~a "feeds/all.atom.xml")
              #:rss-path    (~a "feeds/all.rss.xml")
              #:contents    contents))
