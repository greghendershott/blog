#lang at-exp racket/base

(require racket/require
         (multi-in racket (contract file format match path))
         (only-in "compile-post.rkt" datetime+tags->xexpr)
         "post.rkt"
         "render-page.rkt"
         "tag-posts.rkt"
         "util.rkt"
         "xexpr.rkt")

(module+ main (main))

(define (main)
  (match (current-command-line-arguments)
    [(vector tag-file www output-html)
     (define tag (path->string (file-name-from-path tag-file)))
     (define the-posts (tag-file->sorted-posts tag-file))
     (make-directory* (path-only output-html))
     (call-with-output-file*/delete
      #:exists 'replace output-html
      (λ (out)
        (display "<!DOCTYPE html>" out)
        (display (xexpr->string
                  (index-xexpr tag the-posts (file->uri www output-html)))
                 out)))]))

(define/contract (index-xexpr tag the-posts page-path)
  (-> string? (listof (cons/c path-string? post?)) path-string? xexpr/c)
  (define articles
    (for/list ([the-post (in-list the-posts)])
      (match-define (cons rktd (post title datetime tags blurb more? body)) the-post)
      (define href (~a "/" (sans-top-dir
                            (path-replace-extension rktd #".html"))))
      `(article ([class "index"])
        (header ()
         (h2 () (a ([href ,href]) ,title))
         ,(datetime+tags->xexpr datetime tags))
        ,@blurb
        ,@(if more?
              `((footer () (a ([href ,href]) (em () hellip "More" hellip))))
              `()))))
  (define-values (page-title contents)
    (if (equal? tag "all")
        (values "Home: Greg Hendershott"
                articles)
        (values (~a "Posts tagged \"" tag "\"")
                (cons `(h1 () "Posts tagged " (em ,tag))
                      articles))))
  (page-xexpr #:title       page-title
              #:keywords    tag
              #:description page-title
              #:page-path   page-path
              #:atom-path   (~a "feeds/" tag ".atom.xml")
              #:rss-path    (~a "feeds/" tag ".rss.xml")
              #:contents    contents))
