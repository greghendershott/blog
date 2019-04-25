    Title: Supporting multi-in
    Date: 2019-04-25T00:00:00Z
    Tags: Racket, Emacs

In [racket-mode] I improved support for the `multi-in`[racket] form
provided by `racket/require`[racket].

[racket-mode]: https://github.com/greghendershott/racket-mode/

<!-- more -->

# What is `multi-in`[racket]?

Instead of:

```racket
(require net/uri-codec
         net/url
         racket/contract
         racket/format
         racket/string)
```

You can say:

```racket
(require racket/require
         (multi-in net (uri-codec url))
         (multi-in racket (contract format string)))
```

One detail: The `racket/require` must appear before any `multi-in`
forms. Any sorting must make an exception for this.

# What are the `racket-{tidy trim base}-requires` commands?

- `racket-tidy-requires` gathers multiple `require` forms into one.
  Within that, it groups phase level sub-forms such as `for-syntax`.
  Finally it sorts absolute module paths like `foo` before relative
  paths like `"foo.rkt"`, and sorts each of those alphabetically.

- `racket-trim-requires` uses the `show-requires`[racket] function
  from `macro-debugger/analysis/check-requires`[racket] to analyze
  requires and delete any unused. Also it tidies.

- `racket-base-requires` uses the analysis to change a `#lang racket`
  file to `#lang racket/base`, adding requires as necessary. Also it
  trims and tidies.

# What are the changes?

1. Sorting makes sure to keep `racket/require` first.

2. If `racket/require` is present in the original `require` form(s),
   then `multi-in` is used when tidying.

3. Any analysis that says requires should be removed, should handle
   `multi-in` forms.

Essentially, the commands first "expand" or "explode" `multi-in` forms
to individual requires, do any analysis and modifications, then try to
"unexpand" or "implode" them back again.[^1]

[^1]: Not everything survives a round-trip, exactly. A Cartesian product like `(multi-in (a b) (c d))` will end up as a `(multi-in a (c d))` and a `(multi-in b (c d))` -- equivalent but not as concise.

All together, these changes close issues [355], [356], and [369].

[355]: https://github.com/greghendershott/racket-mode/issues/355
[356]: https://github.com/greghendershott/racket-mode/issues/356
[369]: https://github.com/greghendershott/racket-mode/issues/369


## Example without `racket/require`

Here's an example where `racket/require` is _not_ in the original
`require` forms:

```racket
#lang racket

(require net/url)
(require net/uri-codec)

;; Just some expressions using imported definitions
string-join ~a get-pure-port uri-decode (match-define (list n) (list 1))
```

After <kbd>M-x</kbd> `racket-base-requires`:

```racket
#lang racket/base

(require net/uri-codec
         net/url
         racket/format
         racket/match
         racket/string)

;; Just some expressions using imported definitions
string-join ~a get-pure-port uri-decode (match-define (list n) (list 1))
```

## Example with `racket/require`

Here's an example where `racket/require` _is_ in the original
`require` forms:

```racket
#lang racket

(require racket/require) ;new
(require net/url)
(require net/uri-codec)

;; Just some expressions using imported definitions
string-join ~a get-pure-port uri-decode (match-define (list n) (list 1))
```

After <kbd>M-x</kbd> `racket-base-requires`:

```racket
#lang racket/base

(require racket/require
         (multi-in net (uri-codec url))
         (multi-in racket (format match string)))

;; Just some expressions using imported definitions
string-join ~a get-pure-port uri-decode (match-define (list n) (list 1))
```
