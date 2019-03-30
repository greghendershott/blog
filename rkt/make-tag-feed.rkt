#lang at-exp racket/base

(require racket/require
         (multi-in racket (date file format function match path string))
         threading
         "post.rkt"
         "site.rkt"
         "tag-posts.rkt"
         "util.rkt"
         "xexpr.rkt")

(module+ main (main))

(define (main)
  (match (current-command-line-arguments)
    [(vector tag-file (and which (or "atom" "rss")) output-xml)
     (define tag (path->string (file-name-from-path tag-file)))
     (define the-posts (tag-file->sorted-posts tag-file))
     (make-directory* (path-only output-xml))
     (call-with-output-file*/delete
      #:exists 'replace output-xml
      (curry write-feed which tag the-posts))]))

(define (write-feed which tag the-posts out)
  (match which
    ["atom" (write-atom-feed tag the-posts out)]
    ["rss"  (write-rss-feed  tag the-posts out)]))

(module+ test
  (require rackunit))

;;; atom

(define (write-atom-feed tag the-posts out)
  (define title @~a{Posts tagged "@tag"})
  (define updated
    (match the-posts
      [(cons (cons _rktd x) _) (rfc-8601/universal (post-datetime x))]
      [_ "N/A"]))
  (define xe
    `(feed
      ([xmlns "http://www.w3.org/2005/Atom"]
       [xmlns:dc "http://purl.org/dc/elements/1.1/"]
       [xml:lang "en"])
      (title ([type "text"]) ,title)
      (link ([rel "self"]
             [href ,(full-uri (~a "feeds/" tag ".atom.xml"))]))
      (link ([href ,(full-uri (~a "tags/" tag ".html"))]))
      (id ,(urn tag))
      ;; (etag () ???)
      (updated ,updated)
      ,@(for/list ([x (in-list the-posts)])
          (post->atom-feed-entry-xexpr tag x))))
  (display #"<?xml version=\"1.0\" encoding=\"utf-8\"?>" out)
  (displayln (string->bytes/utf-8 (xexpr->string xe)) out))

(define (post->atom-feed-entry-xexpr tag x)
  (match-define (cons rktd (post title datetime tags blurb more? body)) x)
  (define href (~a "/" (sans-top-dir
                        (path-replace-extension rktd #".html"))))
  `(entry
    ()
    (title ([type "text"]) ,title)
    (link ([rel "alternate"]
           [href ,href]))
    (id ,(urn href))
    (published ,(rfc-8601/universal datetime))
    (updated ,(rfc-8601/universal datetime))
    (author (name ,(author)))
    (content ([type "html"]) ,(xexpr->string `(div () ,@body)))))

;;; rss

(define (write-rss-feed tag the-posts out)
  (define title @~a{Posts tagged "@tag"})
  (define updated
    (match the-posts
      [(cons (cons _rktd x) _) (rfc-8601->rfc-822 (post-datetime x))]
      [_ "N/A"]))
  (define href (full-uri (~a "tags/" tag ".rss.xml")))
  (define xe
    `(rss ([version "2.0"]
           [xmlns:atom "http://www.w3.org/2005/Atom"]
           [xmlns:dc "http://purl.org/dc/elements/1.1/"])
          (channel
           (title ,title)
           (description ,title)
           (link ,href)
           ;; <https://validator.w3.org/feed/docs/warning/MissingAtomSelfLink.html>
           (atom:link ([href ,href]
                       [rel "self"]
                       [type "application/rss+xml"]))
           (lastBuildDate () ,updated)
           (pubDate ,updated)
           (ttl "1800")
           ,@(for/list ([x (in-list the-posts)])
               (post->rss-feed-entry-xexpr tag x)))))
  (display #"<?xml version=\"1.0\" encoding=\"utf-8\"?>" out)
  (displayln (string->bytes/utf-8 (xexpr->string xe)) out))

(define (post->rss-feed-entry-xexpr tag x)
  (match-define (cons rktd (post title datetime tags blurb more? body)) x)
  (define href (~a "/" (sans-top-dir
                        (path-replace-extension rktd #".html"))))
  `(item
    (title ,title)
    (link ,href)
    (guid ([isPermaLink "false"]) ,(urn href))
    (pubDate ,(rfc-8601->rfc-822 datetime))
    ;; Not author: <https://validator.w3.org/feed/docs/error/InvalidContact.html>
    (dc:creator ,(author))
    (description ,(xexpr->string `(div () ,@body)))))

;;; Datetime conversion

;; See rfc-8601->date for description of handling of time zones.
(define (rfc-8601/universal s)
  (~> s rfc-8601->date date->rfc-8601))

;; See rfc-8601->date for description of handling of time zones.
(define (rfc-8601->rfc-822 s)
  (~> s rfc-8601->date date->rfc-822))

;; Accepts 8601 time strings that either:
;;
;; (a) Explicitly have "Z", i.e. Universal Time.
;;
;; (b) Lack a "Z", in which case it's assumed to be local time and
;; converted to Universal Time.
(define (rfc-8601->date s)
  (match s
    [(pregexp "(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})(Z?)"
              (list _ year month day hour minute second Z?))
     (define zulu? (match Z? ["Z" #t] [_ #f]))
     (define d (date (string->number second)
                     (string->number minute)
                     (string->number hour)
                     (string->number day)
                     (string->number month)
                     (string->number year)
                      0 0 #f 0))
     (cond [zulu? d]
           [else (local->universal d)])]
    [other
     (raise-argument-error
      'rfc-8601->822
      "date in ISO 8601 (YYYY-MM-DDThh:mm:ss) format" 0 s)]))

(define (local->universal d)
  (~> d (date->seconds #t) (seconds->date #f)))

(define MONTHS
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
(define DAYS
  #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

(define (date->rfc-822 d)
  (match d
    [(date sc mn hr dy mo yr wd yd dst? tzo)
     (~a (vector-ref DAYS wd) ", "
         (2d dy) " " (vector-ref MONTHS (sub1 mo)) " " yr " "
         (2d hr) ":" (2d mn) ":" (2d sc) " UT")]))

(define (date->rfc-8601 d)
  (match d
    [(date sc mn hr dy mo yr wd yd dst? tzo)
     (~a yr "-" (2d mo) "-" (2d dy)
         "T"
         (2d hr) ":" (2d mn) ":" (2d sc)
         "Z")]))

(define (2d n)
  (cond [(< n 10) (format "0~a" n)]
        [else     (format "~a" n)]))

(module+ test
  ;; Tests where timezone is already explicitly UT.
  (check-equal? (rfc-8601/universal "2014-06-01T00:00:00Z")
                "2014-06-01T00:00:00Z")
  (check-equal? (rfc-8601->rfc-822 "2014-06-01T00:00:00Z")
                "Sun, 01 Jun 2014 00:00:00 UT")
  (check-equal? (rfc-8601/universal "2014-10-11T00:00:00Z")
                "2014-10-11T00:00:00Z")
  (check-equal? (rfc-8601->rfc-822 "2014-10-11T00:00:00Z")
                "Sun, 11 Oct 2014 00:00:00 UT")

  ;; Tests where timezone is unspecified. Assume local time and
  ;; convert to UT. NOTE: These particular tests only work when run
  ;; from a machine where local timezone is ET.
  (match (date*-time-zone-name (current-date))
    [(or "EST" "EDT")
     ;; A date during EDT, 4 hour time difference from UT
     (define EDT-date "2014-06-01T00:00:00")
     (check-equal? (rfc-8601/universal EDT-date)
                   "2014-06-01T04:00:00Z")
     (check-equal? (rfc-8601->rfc-822 EDT-date)
                   "Sun, 01 Jun 2014 04:00:00 UT")
     ;; A date during EST, 5 hour time difference from UT
     (define EST-date "2014-10-11T00:00:00")
     (check-equal? (rfc-8601/universal EST-date)
                   "2014-10-11T04:00:00Z")
     (check-equal? (rfc-8601->rfc-822 EST-date)
                   "Sat, 11 Oct 2014 04:00:00 UT")]
    [_ (void)]))
