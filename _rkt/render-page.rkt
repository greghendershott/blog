#lang racket/base

(require racket/require
         (multi-in racket (contract format))
         xml/xexpr)

(provide page-xexpr)

(define/contract (page-xexpr #:title       title
                             #:description description
                             #:keywords    keywords
                             #:page-path   page-path
                             #:atom-path   atom-path
                             #:rss-path    rss-path
                             #:contents    contents)
  (-> #:title       string?
      #:description string?
      #:keywords    string?
      #:page-path   string?
      #:atom-path   string?
      #:rss-path    string?
      #:contents    (listof xexpr/c)
      xexpr/c)
  `(html ([lang "en"])
         (head ()
               (meta ([charset "utf-8"]))
               (title () ,title)
               (meta ([name "description"] [content ,description]))
               (meta ([name "author"] [content "Greg Hendershott"]))
               (meta ([name "keywords"] [content ,keywords]))
               (meta ([name "viewport"] [content "width=device-width, initial-scale=1.0"]))
               (link ([rel "icon"] [href "/favicon.ico"]))
               (link ([rel "canonical"] [href ,(full-uri page-path)]))
               (link ([rel "stylesheet"] [type "text/css"] [href "main.css"]))
               (link ([rel "alternate"]
                      [type "application/atom+xml"]
                      [title "Atom Feed"]
                      [href ,(full-uri atom-path)]))
               (link ([rel "alternate"]
                      [type "application/rss+xml"]
                      [title "RSS Feed"]
                      [href ,(full-uri rss-path)])))
         (body ()
               (header ())
               (main ()
                     ,@contents)
               (footer ()))))

(define (full-uri uri-path)
  (~a "https://www.greghendershott.com/" uri-path))
