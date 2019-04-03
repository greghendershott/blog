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
         (multi-in racket (match path))
         (multi-in web-server (dispatchers/dispatch servlet-env)))

(module+ main (main))

(define (main)
  (match (current-command-line-arguments)
    [(vector index.html)           (preview index.html #:launch-browser? #f)]
    [(vector index.html "browser") (preview index.html #:launch-browser? #t)]))

(define (preview index.html #:launch-browser? launch-browser?)
  (serve/servlet (lambda (_) (next-dispatcher))
                 #:servlet-path      "/index.html"
                 #:extra-files-paths (list (or (path-only index.html)
                                               (current-directory)))
                 #:listen-ip         "127.0.0.1"
                 #:port              3000
                 #:launch-browser?   launch-browser?))
