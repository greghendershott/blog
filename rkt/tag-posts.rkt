#lang racket/base

(require racket/require
         (multi-in racket (contract file format match path string))
         "post.rkt")

(provide tag-file->sorted-posts)

(define (tag-file->sorted-posts tag-file)
 (define the-posts (for/list ([rktd (in-list (file->lines tag-file))])
                     (cons rktd (call-with-input-file* rktd read))))
  (sort the-posts
        #:key (compose post-datetime cdr)
        string>?))

#;
(map (compose post-datetime cdr)
     (parameterize ([current-directory ".."])
       (tag-file->sorted-posts ".cache/tags/Racket")))
