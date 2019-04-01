#lang at-exp racket/base

(require racket/require
         (multi-in racket (contract file format list match path port string))
         threading
         "post.rkt"
         "render-page.rkt"
         "util.rkt"
         "xexpr.rkt")

(module+ main (main))

(define (main)
  (match (current-command-line-arguments)
    [(vector rktd www output-html)
     (define the-post (call-with-input-file* rktd read))
     (make-parent-directory* output-html)
     (call-with-output-file*/delete
      #:exists 'replace output-html
      (λ (out)
        (displayln "<!DOCTYPE html>" out)
        (displayln (xexpr->string
                    (post-xexpr the-post (file->uri www output-html)))
                   out)))]))

(define (post-xexpr the-post page-path)
  (match-define (post title date tags blurb more? body) the-post)
  (page-xexpr #:title       title
              #:keywords    (string-join tags)
              #:description (xexprs->description blurb)
              #:page-path   page-path
              #:atom-path   "feeds/all.atom.xml"
              #:rss-path    "feeds/all.rss.xml"
              #:contents    body))
