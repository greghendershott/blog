    Title: Hands-on with Clojure
    Date: 2014-10-08T10:01:59
    Tags: Clojure, Hacker School

Until now, Clojure has been an "armchair" language for me. I've read a
lot _about_ it, and I've read a fair amount of code written _in_ it.
But aside from typing a few things into an online REPL like
[Try Clojure](http://tryclj.com/), I've not really used it hands-on.

A couple days ago I started my 12-week batch at [Hacker School]. First
on my list is to get some real experience with Clojure.

[Hacker School]: https://www.hackerschool.com/

And I'm going to blog about the experience.

<!-- more -->

---

A couple caveats:

1. Hacker School encourages you to blog early, blog often. This post
   won't necessarily be super well thought-out. Expect me to write
   some really dumb Clojure code, and state some silly opinions about
   Clojure.

2. I'm coming from a few years of using [Racket] -- and especially
   during the last year feeling very comfortable and reasonably
   proficient. Obviously Clojure is not Racket, in ways that are
   probably some mix of better, worse, and just different. But I may
   get cranky about a few things. I expect some of that will turn out
   to be "early days" and I'll feel better later.

[Racket]: http://www.racket-lang.org

In other words: I welcome comments that help me understand how to do
something better or more idiomatically. However please cut me some
slack on my subjective reactions, because I'm not trying to flame,
here.

---

# JVM

Back when there was that security issue with the JVM on OS X I removed
the JVM from my MacBook. The fact that Clojure uses the JVM is a huge
plus for some people. For me, it's meh. Anyway, no choice. First order
of business is to install it.

I decide to download JDK 8. Later I hear that wasn't necessarily the
best choice:

<https://twitter.com/technomancy/status/519328722324049920>

Amusingly I mistakenly choose to download to `~/Dropbox` not
`~/Downloads`. Although I think oracle.com was slow, it probably
didn't help matters that I was re-uploading the 200 MB back to
Dropbox. Derp.

While waiting for the download to complete, I spend time re-working
through the early chapters of [Learn You a Haskell for Great Good],
since I plan to dive into Haskell and/or Idris in later weeks.

[Learn You a Haskell for Great Good]: http://learnyouahaskell.com/

After I installed the JDK I tried:

```sh
$ java -version
java version "1.8.0_20"
Java(TM) SE Runtime Environment (build 1.8.0_20-b26)
Java HotSpot(TM) 64-Bit Server VM (build 25.20-b23, mixed mode)
```

Erm. What. Version 1.8 doesn't sound like 8. Also `which` and `ll`
show file dates that don't seem new enough.

So apparently I already had JRE on here, despite removing it (I
thought) back around the time of the OS X security bug. Or, is JDK 8
the "8" in JRE 1.8? Huh??

I spend some time trying to figure this out, with no luck.

Eventually I need to find where JDK 8 actually was installed, and make
sure it's in my PATH.

But meanwhile I decide to try ignoring it, and forge ahead and install
`lein` and Clojure.

I followed part of
[A Brief Beginner's Guide to Clojure](http://www.unexpected-vortices.com/clojure/brief-beginners-guide/development-env.html).
I went through its examples using `lein` to create a new project,
start a REPL, and so on. Worked fine.

# Clojure and Emacs

Next, how to get this working in Emacs?

I found [Emacs and Clojure], a guide
for getting set up with Clojure on Emacs. Following that worked out
pretty well. Just a couple problems of my own creation.

[Emacs and Clojure]: http://clojure-doc.org/articles/tutorials/emacs.html

Initially, `cider-jack-in` would complain that `pretty-regexp-alist`
wasn't defined. Some searching turned up
[issue 833](https://github.com/clojure-emacs/cider/issues/833). Yep, I
had an old clojure-mode from doinking around with just editing clojure
code in the past. So I updated clojure-mode.

Now, <kbd>C-c M-j</kbd> cider-jack-in is working. I find it weird that
I must both 1. <kbd>C-c C-k</kbd> to compile the buffer[^1], then 2.
<kbd>C-c M-n</kbd> to enter the namespace... and only then can I use
the REPL. This seems awkward compared to just <kbd>F5</kbd> in
racket-mode. Is there a good reason for separating the two, compiling
vs. entering the ns? Probably. Anyway, I suppose I could bind
<kbd>F5</kbd> in clojure-mode to the composition of compile and enter.
For now, I just learn to type both when needed.

[^1]: Also, <kbd>C-c C-x</kbd> "refreshes" REPL with code from current
buffer. At the moment this feels like a distinction without a
difference.

So things are mostly working, except two things:

- I try to use <kbd>C-c C-d d</kbd> (to see documentation) but it
errors: `Wrong type argument: stringp, nil`. Huh.

- I try to use <kbd>M-.</kbd>. to visit source, but it says, `No source
  location`. Huh.

Well, maybe I should have paid attention to those warnings about
mismatched versions. I had ended up with `cider 0.8.0-SNAPSHOT` from
MELPA, but `cider-nrepl 0.7.0` from copy-pasta. Eventually I figured
out how to create a `~/lein/profiles.clj` and set it to

```clj
{:user {:plugins [[cider/cider-nrepl "0.8.0-SNAPSHOT"]]}}
```

Now all seems well. Overall [Emacs and Clojure] really helped me
understand the pieces. Big thanks to the author, Gareth Jones, and the
other people who contributed it.

# My First Project, and `require`

From my armchair Clojure I suspected I'd have some "uncanny valley"
moments, where Clojure and Racket seem the same but are subtly
different. I wasn't disappointed. Right off the bat I struggled with
`require`. I was trying to write `penultimate`. At first I thought, oh
in Racket I'd use `match` for this. So I tried to require
`core.match`. What I didn't realize is that Clojure's `:require`
imports things under a name with a namespace prefix, by default.

I added it to `:dependencies` in my `project.clj`:

```clj
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.match "0.2.1"]]
```

That's fine. But trying to require it in my `core.clj`:

```clj
(ns my-lib.core
  (:require [clojure.core.match :only (match)]))
```

Gives an error:

```
=> #<CompilerException java.io.FileNotFoundException: Could not locate clojure/core/match__init.class or clojure/core/match.clj on classpath: , compiling:(my_lib/core.clj:1:1)>
```

And I have no idea really what the hell this means or what to do about it.

I gave up on core.match, realizing I could do what I want with
Clojure's destructuring `let`. Which is awesome sauce. But then, I
wanted to make some unit tests. And I had the same problem trying to
use `clojure.test`.

Eventually I realized that if I wanted to refer to `deftest` and `is`
imported from the `clojure.test` namespace, I had to do it either of
two ways:

1. Use `(:require [clojure.test :refer :all])` so I could refer to
   `deftest` and `is` without any namespace prefix.

2. Use `(:require clojure.test)` and prefix e.g.
   `clojure.test/deftest` and `clojure.test/is`.

In other words, by default, Clojure's `require` is backwards from
Racket's with respect to name prefixes. I'm not sure I really like
Clojure's choice here. In my not-insubstantial experience with Racket,
name collisions are infrequent. They seem like the special case that
should require (no pun) special work -- not the other way around.
(When name collisions do occur in Racket, it's easy to use Racket's
`only-in`, and/or use `prefix-in` to create some prefix. You could
even do `(prefix-in foo/)` to make prefixes in a superficial Clojure
style).

# Tests

After I managed to get a `deftest` in my `core.clj` file, I noticed
the `../test/my_lib/core_test.clj` file created by Leinengen. Ah. The
tests are supposed to go there. And if I put them there, `lein test`
and `cider-run-all-tests` will work.

So that's good, but I don't love is that I _must_ put tests in a
separate file. I've really grown to like Racket's submodules, which
enable things like putting things in a submodule named `test`, in the
same file as what's being tested.[^2]

[^2]: To clarify, the neat thing about a `test` _submodule_ in Racket
is that it exists at "test time". In other words, the submodule can
live in the same source file, and access its surrounding parent
module. But it is not loaded or evaluated at run time -- just at "test
time". Likewise you can use submodules for "documentation time" or
other concepts.

# Code

Here's what I ended up with right before it was time to head out for
dinner:

```clj
(ns my-lib.core
  (:require [clojure.test :refer :all]))

;; A (probably dubious) implementation of penultimate.
(defn penultimate-dubious [xs]
  (let [[x & xs] xs]
    (if xs
      (let [[y & xs] xs]
        (if-not xs
          ;; y is last, therefore x must be next-to-last
          x
          ;; keep trying
          (recur (cons y xs))))
      nil)))

(defn penultimate [xs]
  (let [[x & [y & ys]] xs]
    (if ys
      (recur (cons y ys))
      (and y x))))

;; See ../test/my_lib/core_test.clj for tests. Unfortunately nothing
;; like Racket's test submodules.
```

Although `penultimate` seems OK to me, I'm curious about other ways to
express that in Clojure.

# Conclusions and next steps

What conclusions can we draw from all this? Nothing too important.
This is just a brain dump of my experience and opinions. It's probably
fair to say that the "onboarding" for Racket is simpler, in terms of
getting a working development environment, writing and evaluating
code. Beyond that, I have some knee-jerk reactions that probably don't
mean much.

The 4clojure.com problems are great, and I will work on more today.
However I'm already feeling the urge to pick some small "real"
project, and work on that instead of (or at least in addition to)
problem sets. The trick will be to pick something that's not too big
or small to tackle.
