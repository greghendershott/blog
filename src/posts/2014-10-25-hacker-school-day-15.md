    Title: Hacker School day 15
    Date: 2014-10-25T11:03:17
    Tags: Hacker School

I spent most of late Thursday and Friday working on open source
projects that pre-date Hacker School.

<!-- more -->

- Racket wanted to accept a [pull request] from me to fix a bug with
  small bit-vectors. But because I had stumbled around before finding
  the correct solution, and discussion on the PR, I'd accumulated
  about 8 commits on the PR branch that needed to be squashed. Also,
  the PR had been open for about 8 weeks, so I needed to fetch Racket
  HEAD and build in order to test properly. Finally I needed to wait
  for Travis CI to finish (the full builds of Racket there take ~30
  minutes). In all, a fair amount of busywork, but that's an important
  part of the process.

[pull request]: https://github.com/plt/racket/pull/756

- I fixed a [bug][markdown-bug] in my markdown parser. After boiling
  it down to the minimal example, the problem was obvious; the parser
  was sometimes returning an x-expression at a stage where a raw
  `string?` was expected -- in the case of an `<!-- HTML comment -->`
  inside a list item. The fix was simply deleting the check for the
  HTML comment. Which made me suspicious. Had there been some intent
  behind the check, even though it now seemed crazy? Or had the check
  just been a brain fart? I decided it was the latter, after thinking
  it through, and also seeing that all the unit tests (of which I have
  many) still passed. So I added the fix, plus a couple regression
  tests, [committed], pushed, and commented/closed the issue on GitHub.

[markdown-bug]: https://github.com/greghendershott/frog/issues/106
[committed]: https://github.com/greghendershott/markdown/commit/0c2e8f8a4b1aa8df2d3ed2955f16553628ac4e08

- I fixed a [bug][racket-mode-bug] in my racket-mode for Emacs. Turns
  out it wasn't treating Unicode characters as symbol constituents,
  causing a problem with some paredit operations. Fortunately I had
  inherited the problem from scheme-mode, and knew that the problem
  did _not_ occur in lisp-mode. As a result, it was straightforward to
  track down. The fix was simply setting `multibyte-syntax-as-symbol`
  to `t`.

[racket-mode-bug]: https://github.com/greghendershott/racket-mode/issues/54

- I reviewed a [pull request for `#lang rackjure`][PR] regarding how a
  Racket `#lang` could install a readtable for the REPL, without
  doinking the readtable for other modules.

[PR]: https://github.com/greghendershott/rackjure/pull/46

Of the three bugs, two turned out to be one-line fixes -- even though
they took some time to figure out and to do "paperwork" like bug
trackers and regression tests. On the one hand, such bugs feel like a
low signal:noise ratio. On the other hand, they're satisfying because
they make you feel like the overall design/assumptions were good.

Also I did some work implementing Racket variants of some Clojure
map-related functions like `get`, `get-in`, `assoc-in`, `update-in`,
`partition`, `take`,[^take] and `juxt`. More trivially, alias things
like `every?`, `some`, `assoc`, and `dissoc` to their Racket
equivalents. Although I haven't yet pushed these to `#lang rackjure` I
may do so over the weekend.

---

That was day 15 of about 60. On the one hand, it feels like a very
long 3 weeks (in a good way). On the other hand, I can't believe I'm
already 25% through.

Time perceptions, how do they work?[^magnets]

[^take]: Unlike Racket's, this doesn't raise an exception when there
are fewer than N elements left, it just returns them. Which IMHO is
more useful; in the past I'd written a `take<=`.

[^magnets]: Also: Magnets.
