#lang racket/base

(require racket/require
         (multi-in racket (contract date format))
         xml/xexpr
         "site.rkt")

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
     (link ([rel "stylesheet"] [type "text/css"] [href "/main.css"]))
     (link ([rel "alternate"]
            [type "application/atom+xml"]
            [title "Atom Feed"]
            [href ,(full-uri atom-path)]))
     (link ([rel "alternate"]
            [type "application/rss+xml"]
            [title "RSS Feed"]
            [href ,(full-uri rss-path)])))
    (body ()
     ,(header)
     (main ([class "site"]) ,@contents)
     ,(footer))))

(define (header)
  `(header ([class "site"])
    (nav
     (ul
      (li (a ([href "/"]) "Greg Hendershott"))
      (li (a ([href "/tags/index.html"]) "Tags"))
      (li (a ([href "/feeds/all.atom.xml"]) "Atom"))
      (li (a ([href "/feeds/all.rss.xml"]) "RSS"))))))

(define (footer)
  `(footer ([class "site"])
    (p ()
     "Created using a Makefile, Racket, and 'tadpole'.")
    (p ()
     "Copyright " copy " 2012" ndash ,(~a (date-year (current-date)))
     " by Greg" nbsp "Hendershott. All rights reserved.")))
