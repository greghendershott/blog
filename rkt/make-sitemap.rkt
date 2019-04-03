#lang racket/base

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
         (multi-in racket (contract file format match path string))
         "site.rkt"
         "tag-posts.rkt"
         "util.rkt")

(module+ main (main))

(define (main)
  (match (current-command-line-arguments)
    [(vector sitemap.txt) (write-sitemap sitemap.txt)]))

(define (write-sitemap sitemap.txt)
  (call-with-output-file*/delete
   #:exists 'replace sitemap.txt
   (λ (out)
     (for ([path (in-list (directory-list (path-only sitemap.txt)))]
           #:when (and (equal? #".html" (path-get-extension path))
                       (not (directory-exists? path))))
       (displayln (full-uri (path->string path))
                  out)))))
