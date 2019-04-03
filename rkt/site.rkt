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

(require racket/contract
         racket/format
         net/uri-codec)

(provide author
         scheme
         host
         full-uri
         urn)

(define (author)   "Greg Hendershott")
(define (scheme)   "https")
(define (host)     "www.greghendershott.com")
(define (host/urn) "urn:www-greghendershott-com")

(define/contract (full-uri uri-path)
  (-> (and/c path-string? relative-path?) string?)
  (~a (scheme) "://" (host) "/" uri-path))

(define (urn uri-path)
  ;; Note that URNs have a more restricted syntax than URIs.
  (~a (host/urn) ":" (regexp-replace* #px"/" uri-path "-")))
