#lang at-exp racket/base

(require racket/require
         (multi-in racket (file format match path)))

(module+ main (main))

(define (main)
  (match (current-command-line-arguments)
    [(vector tag-file output-xml)
     (make-directory* (path-only output-xml))
     ;;TODO: For real
     (copy-file tag-file output-xml #t)]))
