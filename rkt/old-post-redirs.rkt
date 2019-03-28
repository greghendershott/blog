#lang racket/base

(require racket/require
         (multi-in racket (format match))
         (multi-in aws (keys s3)))

(module+ main (main))

(define (main)
  (match (current-command-line-arguments)
    [(vector bucket) (put-redirs bucket)]))

(define (put-redirs bucket)
  (parameterize ([aws-cli-profile "greg"])
    (read-keys/aws-cli))
  ;; PUT 0 bytes object with old pathname and
  ;; x-amz-website-redirect-location
  (for ([p (in-list old-posts)])
    (define-values (old new) (old+new p))
    (put/bytes (~a bucket old)
               #""
               ""
               (hasheq 'x-amz-website-redirect-location new))))

(define (old+new post)
  (match post
    [(pregexp "^(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d)-(.+)$"
              (list _ year month day name))
     (values (~a "/" year "/" month "/" name ".html")
             (~a "/" year "-" month "-" day "-" name ".html"))]))

(define old-posts
  (list "2011-12-27-domain-registrar-switch"
        "2012-02-16-google"
        "2012-08-15-ancient-history"
        "2013-01-14-fear-of-macros"
        "2013-01-24-series-a-round-human-worker-theme-park"
        "2013-02-01-computing-like-component-stereo-systems"
        "2013-02-05-compare-apples-to-oranges-with-the-nexus-4"
        "2013-02-07-we-need-a-prior-art-database"
        "2013-02-12-clear-interrupts"
        "2013-02-14-my-chrome-extensions"
        "2013-02-19-walking-in-the-steps-of-soft-interrogation"
        "2013-02-20-fucking-suggested-post-why-web-apps-matter"
        "2013-02-26-greg-head-first-greg"
        "2013-03-08-my-heartless-brainless-politics"
        "2013-03-09-lull-while-i-prepare-to-change-tires"
        "2013-03-10-live-with-frog"
        "2013-03-11-frog-overview"
        "2013-03-11-keyword-structs"
        "2013-03-14-killing-google-reader"
        "2013-03-15-the-year-google-became-evil"
        "2013-03-19-feed-stats-in-frog-without-feedburner"
        "2013-03-22-readability"
        "2013-03-22-serve-static-files"
        "2013-03-26-host-your-own-web-apps"
        "2013-03-29-my-google-reader-successor"
        "2013-04-02-parameters-in-racket"
        "2013-04-05-roger-ebert-not-engines"
        "2013-04-11-planet-vs-the-new-package-system"
        "2013-04-16-a-guide-for-infrequent-contributors-to-racket"
        "2013-05-06-feeds2gmail"
        "2013-05-24-threading-macro"
        "2013-05-28-chromebook-pixel"
        "2013-06-23-you-have-something-to-hide"
        "2013-06-24-a-case-with-fall-through"
        "2013-07-05-using-travis-ci-for-racket-projects"
        "2013-07-25-skim-or-sink"
        "2013-08-03-spoiler-alert-give-google-power-of-attorney"
        "2013-08-03-using-call-input-url"
        "2013-08-27-understanding-and-using-c-pointers"
        "2013-10-10-interview-and-racketcon-talk"
        "2013-11-06-markdown-parser-redesign"
        "2013-12-01-racket-package-management"
        "2014-01-23-using-syntax-loc"
        "2014-01-31-syntax-loc-and-unit-tests"
        "2014-06-02-fallback-when-required-function-not-available"
        "2014-06-02-racket-cookbook"
        "2014-06-03-file-and-line-in-racket"
        "2014-06-19-does-your-racket-project-need-a-makefile"
        "2014-06-22-destructuring-lists-with-match"
        "2014-09-25-written-in-racket"
        "2014-10-08-hands-on-with-clojure"
        "2014-10-09-hands-on-with-clojure-day-2"
        "2014-10-10-hands-on-with-clojure-day-3"
        "2014-10-14-hands-on-with-clojure-day-4"
        "2014-10-21-hands-on-with-clojure-day-5"
        "2014-10-21-why-macros"
        "2014-10-25-hacker-school-day-15"
        "2014-10-27-applicable-symbols"
        "2014-11-03-hands-on-with-haskell"
        "2014-11-14-hacker-school-week-6"
        "2014-11-14-racket-workflow"
        "2014-11-17-github-dropped-pygments"
        "2014-12-05-blogging-catch-up"
        "2015-07-04-keyword-structs-revisited"
        "2015-08-20-at-expressions"
        "2017-02-06-emacs-themes"
        "2017-03-08-please-scroll"
        "2017-04-18-racket-makefiles"
        "2018-05-03-extramaze-llc-using-racket-postgresql-aws-but-no-ads-or-js"
        "2018-05-13-extramaze-llc-using-system-fonts-not-google-fonts"
        "2018-10-03-racket-mode"
        "2018-11-01-thread-names"
        ))
