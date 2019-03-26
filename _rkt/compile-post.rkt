#lang at-exp racket/base

(require racket/require
         (multi-in racket (contract file format list match path string))
         markdown
         threading
         (only-in srfi/1 break)
         (multi-in frog (enhance-body paths))
         xml/xexpr
         "post.rkt"
         "util.rkt"
         "xexpr2text.rkt")

;; 1. Create a .rktd file something like the Frog post-struct.rkt
;;
;; 2. For each tag, create a directory of the tag name (if it doesn't
;;    already exist) and in that tag subdir create a file with the
;;    same basename as the .md source and .rkt.
;;
;; 3. Every post has the implicit tag "all".
;;
;; (This sets things up for later: For each tag, we'll generate an
;; index HTML, an Atom feed, and an RSS feed. Plus tags/all.html is
;; copied to be the site's /index.html.)

(module+ main (main))

(define (main)
  (match (current-command-line-arguments)
    [(vector source rktd)
     (define the-post (read-post (build-path source)))

     (make-parent-directory* rktd)
     (call-with-output-file*/delete
      #:exists 'replace rktd
      (λ (out) (write the-post out)))

     (make-directory* (build-path (path-only rktd) "tags"))
     (for ([tag (in-list (cons "all" (post-tags the-post)))])
       (define tag-file (build-path (path-only rktd) "tags" (slug tag)))
       (call-with-output-file*/delete
        #:exists 'append tag-file
        (λ (out) (displayln rktd out))))]))

(define/contract (read-post source)
  (-> path? (or/c post? #f))
  (define footnote-prefix (string->symbol
                           (path->string
                            (path-replace-extension
                             (file-name-from-path source) #""))))
  (define xs (parse-markdown source footnote-prefix))

  ;; Split to the meta-data and the body
  (match-define (list title datetime tags body) (meta-data xs source))

  ;; Split out the blurb (may be less than the entire body)
  (define-values (blurb more?) (above-the-fold body))

  (post title
        datetime
        tags
        (enhance-body blurb)
        more?
        `((article ()
           (header ()
            (h1 () ,title)
            ,(datetime+tags->xexpr datetime tags))
           ,@(enhance-body body)
           (footer ())))))

(define/contract (enhance-body xs)
  (-> (listof xexpr/c) (listof xexpr/c))
  (~> xs
      (syntax-highlight #:python-executable "python"
                        #:line-numbers? #t
                        #:css-class "source")
      (auto-embed-tweets #:parents? #t)
      (add-racket-doc-links #:code? #t #:prose? #t)))

(define/contract (meta-data xs source)
  (-> (listof xexpr/c) path?
      (list/c string? string? (listof string?) (listof xexpr/c)))
  (define (err x)
    (raise-user-error 'error "~a: Must start with metadata but ~a" source x))
  (define (warn x)
    (printf "~a: Ignoring unknown metadata: ~v" source x))
  (match xs
    [`(,(or `(pre () (code () . ,metas)) ;Markdown
            `(pre () . ,metas)           ;Markdown
            `(pre . ,metas)              ;Markdown
            `(p () . ,metas))            ;Scribble
       . ,more)
     ;; We don't want HTML entities like &ndash;
     (define plain-text (string-join (map xexpr->markdown metas) ""))
     (define h
       (for/fold ([h (hash)])
                 ([s (string-split plain-text "\n")])
         (match s
           [(pregexp "^ *(.+?): *(.*?) *$" (list _ k v))
            #:when (member k '("Title" "Date" "Tags"))
            (hash-set h k v)]
           [s (warn s) h])))
     (define (required k)
       (define (fail [pre ""])
         (err (format "missing ~a~v" pre k)))
       (match (string-trim (hash-ref h k fail))
         ["" (fail "non-blank ")]
         [v  v]))
     (define (optional k)
       (hash-ref h k ""))
     (list (required "Title")
           (required "Date" )
           (append (~>> (optional "Tags")
                        tag-string->tags))
           more)]
    [(cons x _) (err (~a "found:\n" (format "~v" x)))]
    [_ (err "none found")]))

(module+ test
  (require rackunit
           racket/function))

(module+ test
  (define p (string->path "/path/to/file"))
  (test-case "Various HTML \"envelopes\" and HTML entities"
    (check-equal? (meta-data `((pre () (code () "Title: title\nDate: date\nTags: DRAFT\n"))) p)
                  (list "title" "date" '("DRAFT") '()))
    (check-equal? (meta-data `((pre () "Title: title\nDate: date\nTags: DRAFT\n")) p)
                  (list "title" "date" '("DRAFT") '()))
    (check-equal? (meta-data `((pre "Title: title\nDate: date\nTags: DRAFT\n")) p)
                  (list "title" "date" '("DRAFT") '()))
    (check-equal? (meta-data `((p () "Title: title" ndash "hyphen \nDate: date\nTags: DRAFT\n")) p)
                  (list "title-hyphen" "date" '("DRAFT") '())))
  (test-case "Handle spaces (or not) around key: and value"
    (check-equal? (meta-data `((pre "Title:title\nDate:date\nTags:DRAFT\n")) p)
                  (list "title" "date" '("DRAFT") '()))
    (check-equal? (meta-data `((pre " Title:  title  \n  Date: date \nTags: DRAFT  \n")) p)
                  (list "title" "date" '("DRAFT") '())))
  (test-case "Error raised for missing metadata"
    (check-exn #px"missing \"Title\""
               (thunk (meta-data '((pre "")) p)))
    (check-exn #px"missing \"Title\""
               (thunk (meta-data '((p () "")) p))))
  (test-case "Error raised for blank Title or Date -- https://github.com/greghendershott/frog/issues/213"
    (check-exn #px"missing non-blank \"Title\""
               (thunk (meta-data '((pre "Title: \n")) p)))
    (check-exn #px"missing non-blank \"Date\""
               (thunk (meta-data '((pre "Title: Some Title\nDate: \n")) p))))
  (test-case "https://github.com/greghendershott/frog/issues/142"
    (check-equal? (meta-data '((p
                                ()
                                "Title: A Beginner"
                                rsquo
                                "s Scribble Post\nDate: 2013-06-19T00:00:00\nTags: Racket, blogging"))
                             (string->path "/"))
                  (list "A Beginner's Scribble Post"
                        "2013-06-19T00:00:00"
                        '("Racket" "blogging")
                        '())))
  (test-case "https://github.com/greghendershott/frog/issues/189"
    (check-equal? (meta-data `((pre () "Title: title\nDate: date\nTags: \n")) p)
                  (list "title" "date" '() '()))
    (check-equal? (meta-data `((pre () "Title: title\nDate: date\nTags:\n")) p)
                  (list "title" "date" '() '()))
    (check-equal? (meta-data `((pre () "Title: title\nDate: date\n")) p)
                  (list "title" "date" '() '())))
  (test-case "https://github.com/greghendershott/frog/issues/211"
    (check-equal? (meta-data `((pre () "Title: title\nDate: date\nTags:\n")) p)
                  (list "title" "date" '() '()))))

(define (tag-string->tags s)
  (match (regexp-split #px"," (string-trim s))
    ['("") '()]
    [ss    (map string-trim ss)]))

(module+ test
  (check-equal? (tag-string->tags "  ")
                '())
  (check-equal? (tag-string->tags " some, post ,   tags ")
                '("some" "post" "tags")))

(define (above-the-fold xs)
  (define-values (above below) (break more-xexpr? xs))
  (values above (not (empty? below))))

(define (more-xexpr? x)
  (match x
    [`(p ,(pregexp "\\s*<!--\\s*more\\s*-->\\s*")) #t] ;old markdown parser
    [`(!HTML-COMMENT () ,(pregexp "more")) #t]         ;new markdown parser
    [_ #f]))

(module+ test
  (check-true (more-xexpr? `(p   "<!--more-->")))
  (check-true (more-xexpr? `(p " <!-- more -->")))
  (check-true (more-xexpr? `(p "<!--  more  -->")))
  (check-false (more-xexpr? "not more")))

(define (xexprs->string xs)
  (string-join (map xexpr->string xs) "\n"))

(define (tags->xexpr tags)
  `(span ([class "tags"])
         ,@(add-between (map tag->xexpr tags) ", ")))

(define (tag->xexpr s)
  `(a ([href ,(~a "/tags/" (slug s) ".html")])
      ,s))

(define (datetime+tags->xexpr d ts)
  `(p ([class "date-and-tags"])
      ,(datetime->xexpr d)
      " :: "
      ,(tags->xexpr ts)))

(define (datetime->xexpr d)
  (define ymd (substring d 0 10)) ;; just YYYY-MM-DD
  `(time ([datetime ,ymd]
          [pubdate "true"]) ,ymd))
