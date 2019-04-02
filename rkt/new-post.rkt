#lang at-exp racket/base

(require racket/require
         (multi-in racket (date file format function match))
         (only-in frog/paths slug)
         "util.rkt")

(module+ main (main))

(define (main)
  (match (current-command-line-arguments)
    [(vector posts-dir)
     (display "Title: ")
     (define title (read-line))

     (match-define (struct* date ([year y] [month m] [day d])) (current-date))
     (define year (~a y))
     (define (~00 n) (~r n #:min-width 2 #:pad-string "0"))
     (define month (~00 m))
     (define day (~00 d))
     (define 8601-str (~a year "-" month "-" day "T00:00:00Z"))

     (define post-file (build-path posts-dir year month (~a (slug title) ".md")))
     (make-parent-directory* post-file)
     (call-with-output-file*/delete
      #:exists 'error post-file
      (λ (out)
        (displayln
         @~a{    Title: @title
                 Date: @8601-str
                 Tags:

             This is part of the blurb.

             <!-- more -->

             This is the rest.
             }
         out)))
     (displayln @~a{Created @(path->complete-path post-file)})]))

