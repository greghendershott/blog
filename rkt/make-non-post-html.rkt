#lang at-exp racket/base

;; Copyright 2019 by Greg Hendershott.
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; 	http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(require racket/require
         (multi-in racket (contract file format list match path port string))
         (only-in markdown parse-markdown)
         threading
         "page-xexpr.rkt"
         "util.rkt"
         "xexpr.rkt")

;; Don't bother caching these. Directly from .md to .html.

(module+ main (main))

(define (main)
  (match (current-command-line-arguments)
    [(vector non-post-source www output-html)
     (make-parent-directory* output-html)
     (call-with-output-file*/delete
      #:exists 'replace output-html
      (λ (out)
        (display "<!DOCTYPE html>" out)
        (display (xexpr->string
                  (non-post-xexpr (build-path non-post-source)
                                  (file->uri www output-html)))
                 out)))]))

(define (non-post-xexpr source-path page-path)
  (define contents (parse-markdown source-path))
  (page-xexpr #:title       (title contents source-path)
              #:keywords    ""
              #:description ""
              #:page-path   page-path
              #:atom-path   "feeds/all.atom.xml"
              #:rss-path    "feeds/all.rss.xml"
              #:contents    contents))

(define (title xs path)
  (or (for/or ([x (in-list xs)])
        (match x
          ;; First h1 header, if any
          [`(h1 (,_ ...) . ,els)
           (string-join (map xexpr->markdown els) "")]
          [_ #f]))
      ;; Else name of the source file
      (path->string
       (file-name-from-path
        (path-replace-extension path #"")))))
