    Title: Destructuring lists with match
    Date: 2014-06-22T22:20:56
    Tags: Racket

Let's say you need to destructure a list with `match`, using a pattern
that specifies a "rest" or "more" element. Be careful. You probably
want to use `list*` not `list`.

<!-- more -->

> Update 2014-06-25: Sam Tobin-Hochstadt pushed a [commit] that
> changes this. Fast! Thanks! So if you're building from HEAD or using
> a nightly build, this is now moot. However if you're writing code
> for a library that may be used with Racket 6.0.1 or older, it
> remains relevant.

[commit]: https://github.com/plt/racket/commit/dcb5b09a14df4afd729a50b64c67c3c35f716da9

## The long version

I answered a [question on Stack Overflow][question]. Basically, it was
about how to select every 1st and 4th element from a list, repeatedly.
My answer was equivalent to what we shall name `slow`:

[question]: http://stackoverflow.com/questions/24354824/remove-elements-in-a-list-using-a-pattern

```racket
(define (slow xs)
  (match xs
    [(list a _ _ d more ...) (list* a d (slow more))]
    [(cons a _)              (list a)]
    [_                       (list)]))
```

It passed all these tests:

```racket
(define (test f)
  (local-require rackunit)
  ;; Your example:
  (check-equal? (f '(0 1 2 3 4 5 6 7 8 9 10)) '(0 3 4 7 8))
  ;; Other tests:
  (check-equal? (f '())           '())
  (check-equal? (f '(0))          '(0))
  (check-equal? (f '(0 1))        '(0))
  (check-equal? (f '(0 1 2))      '(0))
  (check-equal? (f '(0 1 2 3))    '(0 3))
  (check-equal? (f '(0 1 2 3 4))  '(0 3 4)))
```

Later, I saw that uselpa posted a generalized, elegant
[answer](http://stackoverflow.com/a/24355921/343414) using `for/fold`
and `in-cycle`.

```racket
(define (pattern-filter pat lst)
  (reverse
   (for/fold ([res null])
             ([p (in-cycle pat)]
              [e (in-list lst)])
     (if p (cons e res) res))))
```

At which point a few things happened.

I was impressed by that answer. Wished I'd answered that way. Consoled
myself, well, at least my bespoke pattern-specific version is probably
somewhat _faster_. Right?

After feeling smug for a moment, I had a nagging feeling. Well shit. I
should measure. See how much faster.

I measured. I was horrified how wrong I was.

```racket
(define bench
  (let ([xs (build-list 10000 values)])
    (λ (f)
      (for ([i 3]) (collect-garbage))
      (printf "Timing ~a ... " (object-name f))
      (time (void (f xs))))))

(bench slow)
;; Timing slow ... cpu time: 256 real time: 266 gc time: 46
```

Whereas uselpa's `partition-filter` version was:

```racket
;; Timing partition-filter ... cpu time: 3 real time: 4 gc time: 0
```

WAT.

I rewrote it using `cond`, `car`, etc. -- i.e. what I imagined `match`
would expand to. That _was_ as fast as I'd expected.

I scratched my head for a bit. On a hunch I tried a slightly different
`match` pattern, which we'll name `fast`:

```racket
(define (fast xs)
  (match xs
    [(list* a _ _ d more) (list* a d (fast more))]
    [(cons a _)           (list a)]
    [_                    (list)]))

(bench slow)
;; Timing slow ... cpu time: 256 real time: 266 gc time: 46
(bench fast)
;; Timing fast ... cpu time: 1 real time: 0 gc time: 0
```

The difference?[^1]

```racket
(list  this more ...)  ;; BAD
(list* this more)      ;; GOOD
```

All along I'd believed they're equivalent. They're not.[^quasi]

In fact, if you _double_ the test list length (from 10,000 to 20,000)
the time _quadruples_ (from 256 to about 1000).

## Why?

As Jay McCarthy explained to me on IRC, `(list this more ...)` needs
to test that the input is a `list?`, and `cons` up the elements to
bind to `more`. Whereas `(list* this more)` doesn't care if the input
is a list -- whatever it is just gets bound to `more`.

The `list*` match pattern is like the `list*` function:

```racket
(list* 1 2 (list 3 4 5)) ;; '(1 2 3 4 5)
(list* 1 2 3)            ;; '(1 2 . 3)
(list* 1 2 "whatever")   ;; '(1 2 . "whatever")
```

---

## Final thoughts

1. I should write a blog post about this (done).

2. I should submit a PR with a "Tip!" for the `match` documentation,
   in case this is non-obvious to anyone else.

3. I wonder if `match` could optimize the special case of `(list this
   more ...)` to be `(list* this more)`. Maybe there are subtleties
   (or not-so-subtleties) I'm overlooking. Update 2014-06-25: [done].

4. Although my _corrected_ version _was_ indeed faster than
   `pattern-filter`, it's almost immeasurably so on a list of 10,000
   items. At 100,000 it _starts_ to matter. In many applications, it
   would be N/A. In general, I think uselpa's answer is best.


[^1]: Also I changed the second pattern from `(list a _ ...)` to
`(cons a _)`, although that test is only hit at the end so it hardly
effects the run time in this case.

[^quasi]: By the way, same issue with quasiquote patterns, which for
instance I frequently use to destruct x-expressions. Instead of ``
`(,this ,that ,more ...) `` use `` `(,this ,that . ,more) ``.
