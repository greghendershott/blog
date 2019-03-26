#lang at-exp racket/base

(require racket/require
         (multi-in racket (contract file format match path))
         xml
         "post.rkt"
         "render-page.rkt"
         "util.rkt")

(module+ main (main))

(define (main)
  (match (current-command-line-arguments)
    [(vector tag-file output-html)
     (define the-posts (for/list ([rktd (in-list (file->lines tag-file))])
                         (cons rktd (call-with-input-file* rktd read))))
     (make-directory* (path-only output-html))
     (call-with-output-file*/delete
      #:exists 'replace output-html
      (λ (out)
        (displayln "<!DOCTYPE html>" out)
        (displayln (xexpr->string
                    (index-xexpr the-posts output-html))
                   out)))]))

(define/contract (index-xexpr the-posts page-path)
  (-> (listof (cons/c path-string? post?)) path-string? xexpr/c)
  (page-xexpr
   #:title       "" ; title
   #:keywords    "" ;(string-join tags)
   #:description "" ;(xexprs->description blurb)
   #:page-path   page-path
   #:atom-path   ""
   #:rss-path    ""
   #:contents
   (for/list ([the-post (in-list the-posts)])
     (match-define (cons rktd (post title date tags blurb more? body)) the-post)
     (define href (~a "/" (path->string
                           (file-name-from-path
                            (path-replace-extension rktd #".html")))))
     `(article ()
       (h2 () (a ([href ,href])
               ,title))
       ,@blurb))))
