    Title: A case with fall-through
    Date: 2013-06-24T20:04:01
    Tags: Racket, macros

Jay McCarthy [posted][post] about a macro to do a C-style `case`,
where clauses fall through to the next unless you use a `break`.  His
post is a great look at Racket macrology. Jay's implementation is
elegant. If you haven't yet, [go read it][post].

<!-- more -->

I posted a comment to him:

> One thought about using this in real life: The gotcha with C `switch` statements is omitting the `break` by mistake. It would be safer if `break` were implicit, and fall-through were explicit. Since that is in fact how normal Racket `case` works, I'd worry the mistake would be more likely, here. In other words, what if the macro instead flipped it to where you say `fall-through` (or `next` or `and-also` or whatever) explicitly?
> 
> (I'm tempted to tackle that myself as a follow-on blog post. But in these matters you are the chess master, I'm only playing checkers.)

Guess what? Wisdom did not prevail. I am here to cargo cult my way to
the solution.

What we want to be able to write is the following, using our
hypothetical `case*` and its `and-next` that says to fall through:

```racket
(define printed "")
(define (cas v)
  (set! printed "")
  (case*
   v
   [(1)
    (set! printed (string-append printed "1"))
    (and-next)] ;; fall through to next case clause
   [(2)
    (set! printed (string-append printed "2"))
    2]
   [(3)
    3]))

(check-equal? (cas 1) 2)
(check-equal? printed "12")
 
(check-equal? (cas 2) 2)
(check-equal? printed "2")
 
(check-equal? (cas 3) 3)
(check-equal? printed "")
 
(check-equal? (cas 4) (void))
(check-equal? printed "")
```

Here, the fall-through is explicit, using `and-next`.

The sort of code we'd like our macro to expand that to, would be:

```racket
(define printed "")
(define (cas v)
  (set! printed "")
  (let* ([third-case
          (lambda ()
            3)]
         [second-case
          (lambda ()
            (set! printed (string-append printed "2"))
            2)]
         [first-case
          (lambda ()
            (set! printed (string-append printed "1"))
            (second-case))])
    (case v
      [(1) (first-case)]
      [(2) (second-case)]
      [(3) (third-case)])))

(check-equal? (cas 1) 2)
(check-equal? printed "12")
 
(check-equal? (cas 2) 2)
(check-equal? printed "2")
 
(check-equal? (cas 3) 3)
(check-equal? printed "")
 
(check-equal? (cas 4) (void))
(check-equal? printed "")
```

And the answer is:

```racket
(require (for-syntax racket/syntax
                     syntax/parse)
         racket/stxparam)

(define-syntax-parameter and-next
  (lambda (stx) (raise-syntax-error 'and-next "used outside case*" stx)))
  
(define-syntax (case* stx)
  (syntax-parse stx
    [(_ e:expr [opt body:expr ...+] ...)
     (with-syntax*
         ([(forward-id ...) (generate-temporaries #'(opt ...))]
          [(reverse-id ...) (reverse (syntax->list #'(forward-id ...)))]
          [((reverse-body ...) ...) (reverse (syntax->list #'((body ...) ...)))]
          [(next-id ...) (reverse (cdr (syntax->list #'(forward-id ... void))))])
       #'(let* ([reverse-id
                 (lambda () 
                   (syntax-parameterize ([and-next (make-rename-transformer #'next-id)])
                     reverse-body ...))] ...)
           (case e
             [opt (forward-id)] ...)))]))
```

In Jay's version the `break` syntax parameter meant to use the escape
continuation shared by all clauses.

In this version, the `and-next` syntax parameter varies for each
`case` clause, because it means to fall through to the very next
clause.

As a result, the difference from Jay's version essentially boils down
to `(next-id ...)` being a simple list (not a list of lists), and the
need to `syntax-parameterize` each clause.

---

Digression: Nearly _every_ time I sit down to write a macro with
`syntax-parse`, I forget this magic incantation:

```racket
(require (for-syntax racket/syntax
                     syntax/parse))
```

The annoyance is that the resulting error messages are about how `_`
isn't a valid expression, and/or that the ellipses are wrong.  As
opposed to some message like, "Hey dummy, you're trying to write a
`syntax-parse` macro but you need to `require` a few things."

---

Returning from that digression: The tl;dr of this post is, when a
master like Jay gives me the chess pieces, I can cargo cult them
around my checker board to good effect.

# Update 2013-06-27

In a comment below, Jay suggested that this doesn't need a syntax
parameter. Instead it can check statically for `and-next`.  Plus that
way, errors like passing arguments to `and-next` can be caught at
compile time.

Here is my best shot at that version:

```racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version 2
;;
;; Don't need syntax parameter, just treat `and-next` statically.
;; Also, this means that something like `(and-next 0)` will be a
;; compile time error.

(define-syntax (case* stx)
  (syntax-parse stx
    [(_ e:expr [opt body:expr ...+] ...)
     (with-syntax*
       ([(forward-id ...) (generate-temporaries #'(opt ...))]
        [(reverse-id ...) (reverse (syntax->list #'(forward-id ...)))]
        [((reverse-body ...) ...)
         (for/list ([body (reverse (syntax->list #'((body ...) ...)))]
                    [next (reverse (cdr (syntax->list #'(forward-id ... void))))])
           (syntax-parse body
             [(x ... ((~literal and-next))) #`(x ... (#,next))]
             [(x ... ((~literal and-next) a ...+))
              (raise-syntax-error 'case* "and-next takes no arguments" body)]
             [(x ...) #'(x ...)]))])
       #'(let* ([reverse-id (lambda ()
                              reverse-body ...)] ...)
           (case e
             [opt (forward-id)] ...)))]))
```

This passes all the unit tests, just like the original version.

In addition, if instead of this:

```racket
(case* 0
  [(0) (displayln 0) (and-next)]
  [(1) (displayln 1) #t])
```

You mistakenly supply one or more arguments to `and-next`, you get a
compile error:

```racket
(case* 0
  [(0) (displayln 0) (and-next 0)] ;tsk, tsk
  [(1) (displayln 1) #t])
; case.rkt:2:52: case*: and-next takes no arguments
;  in: ((displayln 0) (and-next 0))
```
 

[post]: http://goo.gl/AWv4o
