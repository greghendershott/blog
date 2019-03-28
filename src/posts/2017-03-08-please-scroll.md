    Title: Please scroll
    Date: 2017-03-08T17:00:00
    Tags: Racket, Emacs

Recently I got more time to catch up on [racket-mode]. I improved two
things that happen to fit one theme -- an extraordinarily advanced UX
concept I call, "scrolling down to the point of interest."

[racket-mode]: https://github.com/greghendershott/racket-mode

<!-- more -->

## Visit definition

In racket-mode you can <kbd>M-.</kbd> (press <kbd>Alt</kbd> and
<kbd>period</kbd>) to visit the definition of a Racket identifier.

There is no Racket library function that supports this, exactly. The
[`identifier-binding`] function gives you a filename, but not a
position within. And actually, it gives you _two_ filenames (and
identifier symbols), because the location and symbol of the
_definition_ might differ from the location and symbol under which it
is _provided_.

[`identifier-binding`]: http://docs.racket-lang.org/reference/stxcmp.html#%28def._%28%28quote._~23~25kernel%29._identifier-binding%29%29

I've also found it can be tricky when something is renamed more than
once -- for example both renamed and wrapped in a contract. Of the
three (or more) names involved, `identifier-binding` will return only
two. For example in `(provide (contract-out [rename orig new
contract]))` it reports (1) the contract wrapper's generated
identifier and (2) `new` -- but _not_ (3) `orig`. Unfortunately the
definition of `orig` is our desired destination.

So, I need to treat `identifier-binding` as a valuable head start
-- but maybe not the real answer.

I need to check each file and try to find the identifier within. This
isn't a job for regular expression; the name might not appear
textually at the definition site (think of `define`-er macros).
Instead I read the file as syntax and walk it. Sometimes it makes
sense to search the syntax after it has been fully-expanded. Sometimes
it helps to walk it unexpanded, looking for some special forms, for
example the "rename" variant of `contract-out`.

If after all that, we can't find the position, racket-mode plops you
at the start of the file.

The change I made was to reduce the chance of that happening. Details
in the [commit] message and diff.

[commit]: https://github.com/greghendershott/racket-mode/commit/c50cd48edc74348bd89b09661ea325dac12fcb48

## View documentation

In racket-mode you can <kbd>C-c C-d</kbd> to view Racket's HTML
documentation in your default web browser. It should (a) open the
correct page and (b) scroll to the item on that page. Unfortunately
(b) didn't always happen on macOS. Under certain conditions, macOS is
reluctant to open `file:` URLs _and_ scroll down to the
anchor/fragment (the bit after the `#` in the URL).

```sh

# Will open the default browser to the top of the define.html page
# but not scroll down to the define-values item:
$ open 'file:///Applications/Racket_v6.7/doc/reference/define.html#%28form._%28%28quote._~23~25kernel%29._define-values%29%29'

# Ditto
$ osascript -e 'open location "file:///Applications/Racket_v6.7/doc/reference/define.html#%28form._%28%28quote._~23~25kernel%29._define-values%29%29"'

# But this works!
$ osascript -e 'tell application "chrome" to open location "file:///Applications/Racket_v6.7/doc/reference/define.html#%28form._%28%28quote._~23~25kernel%29._define-values%29%29"'

# Well, you also want the "activate" at the end:
$ osascript -e 'tell application "chrome" to open location "file:///Applications/Racket_v6.7/doc/reference/define.html#%28form._%28%28quote._~23~25kernel%29._define-values%29%29" activate'

```

Interestingly the generic `open` seems to work fine for `http:`. Also
fine if a `file:` location is under your home directory instead of
`/Applications`. Because security?[^1] Anyway, this is probably why
Racket developers and power users haven't noticed, if they're building
Racket (and its docs) from HEAD.

[^1]: Security theater? I don't see why it's safe to load a page, but risky to scroll to an anchor in it? I also don't see why `/Applications` is more risky than your home dir -- much less some rando `http:` location?

So I can use `osascript` if I know the default browser. How do I know
that? Ugh. OK. This information seems to reside in
`Library/Preferences/com.apple.LaunchServices/com.apple.launchservices.secure.plist`.
Read the JSON, find the correct entry, grab the `browser` part of
`com.company.browser`, and hopefully we're good.

## Pontification

Computer science mostly isn't science.

Software engineering mostly isn't engineering.

Sometimes success is simply scrolling.
