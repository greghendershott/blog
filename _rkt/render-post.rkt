#lang at-exp racket/base

;; 1. Create a .rktd file something like the Frog post-struct.rkt
;;
;; 2. For each tag, create a directory of the tag name (if it doesn't
;;    already exist) and in that tag subdir create a file with the
;;    same basename as the .md source and .rkt.
;;
;; 3. Every post has the implicit tag "all".
;;
;; 4. For each tag, we'll generate an index HTML, an Atom feed, and an
;;    RSS feed. (The "all" tag's things simply get copied, e.g.
;;    /tags/all.html gets copied to /index.html.)

(require racket/require
         (multi-in racket (contract file format list match path port string))
         markdown
         threading
         (only-in frog/paths slug)
         "post.rkt"
         "render-page.rkt"
         "xexpr2text.rkt"
         "util.rkt")

(module+ main (main))

(define (main)
  (match (current-command-line-arguments)
    [(vector rktd output-html)
     (define the-post (call-with-input-file* rktd read))
     (make-parent-directory* output-html)
     (call-with-output-file*/delete
      #:exists 'replace output-html
      (λ (out)
        (displayln "<!DOCTYPE html>" out)
        (displayln (xexpr->string
                    (post-xexpr the-post output-html))
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
