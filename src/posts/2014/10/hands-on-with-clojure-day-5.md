    Title: Hands-on with Clojure day 5
    Date: 2014-10-21T13:54:46
    Tags: Clojure, Hacker School

So I've fallen behind on the blogging, for a few reasons. Time to catch up.

I'm calling this "day 5" as a useful fiction. It's a distillation of
what is closer to days 5-7, or something like that.

As I mentioned before, this series of blog posts is going more
directly from brain to web. Reflection and editing? Not so much.

<!-- more -->

# Clojure port of wffi

I finished what I think is a reasonable initial port of wffi from
Racket to Clojure. Pushed at [clojure-wffi].

[clojure-wffi]: https://github.com/greghendershott/clojure-wffi

The simplest possible example is, given a `horseebooks.md` file like
this:

    # horseebooksipsum.com

    Endpoint: http://horseebooksipsum.com

    # Get

    ## Request
    ````
    GET /api/v1/{paragraphs}
    ````

You can write:

```clj
(defwrappers "horseebooks.md")
(pprint (get {:paragraphs 2}))
```

Which prints:

```clj
{:orig-content-encoding "gzip",
 :trace-redirects ["http://horseebooksipsum.com/api/v1/2"],
 :request-time 190,
 :status 200,
 :headers
 {"Content-Type" "text/plain",
  "Transfer-Encoding" "chunked",
  "Connection" "close",
  "Vary" "Accept-Encoding",
  "Cache-Control" "no-cache",
  "Server" "Apache/2.2.22 (Debian)",
  "Date" "Tue, 21 Oct 2014 18:07:16 GMT"},
 :body
 "Principle to work to make more money while having more fun. Unlucky people.
 And practical explanations. Process from preparation, through to delivery.
 And practical explanations. Process from preparation, through to delivery.
 And practical explanations. And practical explanations. Process from
 preparation, through to delivery. And practical explanations. And practical
 explanations. And practical explanations.\n\nDon't stubbornly. This is a
 very special technique that I have never seen. Don't stubbornly. This is
 a very special technique that I have never seen. And practical explanations.
 Don't stubbornly. Principle to work to make more money while having more fun.
 Unlucky people. Process from preparation, through to delivery. Don't
 stubbornly. Process from preparation, through to delivery. And practical
 explanations. This is a very special technique that I have never seen.
 And practical explanations. And practical explanations.\n\n"}
```

Of course this simple example doesn't show much value-add. But
real-world web services often have numerous parameters allocated among
URL path segments, query parameters, and headers. With wffi, useful
keyword wrapper functions are automatically generated from a markdown
file that both documents and specifies the web service.

If I weren't at Hacker School, I would spend much more time polishing
and refining this. However this project is really just a means to the
end of learning Clojure. So I'm going to force myself to task-switch
to something else, next. I'll return to this project if/when it seems
like the best vehicle to learn more.

# `split-with` and lazy seqs

Previously I posted that `split-with` seems to have an inefficient
implementation.

Needing something like Racket's `splitf-at`, I wrote a quick and dirty
version in Clojure:

```clj
(defn split
  "FIXME: This is the conceptual, inefficient implementation. Should
  reimplement like Racket's splitf-at."
  [pred coll]
  [(take-while pred coll)
   (drop-while pred coll)])
```

This isn't great because it traverses the first portion of the
collection twice.

Someone pointed out that Clojure already provides this. It's called
`split-with`. Nice. But when I <kbd>M-.</kbd>, I see that its
definition is my conceptual one, not the efficient one.

Racket defines `splitf-at` like so:

```racket
(define (splitf-at list pred)
  (unless (procedure? pred)
    (raise-argument-error 'splitf-at "procedure?" 1 list pred))
  (let loop ([list list] [pfx '()])
    (if (and (pair? list) (pred (car list)))
      (loop (cdr list) (cons (car list) pfx))
      (values (reverse pfx) list))))
```

I "ported" this to Clojure like so:

```clj
(defn efficient-split-with
  [pred coll]
  (loop [ps [], coll coll]
    (if (and (seq coll) (pred (first coll)))
      (recur (conj ps (first coll)) (rest coll))
      [ps coll])))
```

One neat thing is the use of `conj` with a `vector` means we don't
have to do the `reverse` like we do in Racket, which should be even
more efficient.

So why does Clojure implement `split-with` the way it does? David
Nolen pointed out that I was forgetting about laziness. Aha.

In connection with this I learned about "chunked sequences" in Clojure
from a [Fogus blog post]. Chunked sequences were added as an
optimization in v1.1. The force granularity was increased from 1 item
to 32.

[Fogus blog post]: http://blog.fogus.me/2010/01/22/de-chunkifying-sequences-in-clojure/

Someone else pointed out that, had transducers been a thing, maybe
lazy seqs wouldn't be needed. (At least not as a default policy. You
could have something like Racket's [`racket/stream`], with laziness
and memoization.)

I already understood, in theory, that side effects expose the
difference between eager and lazy evaluation. I learned, hands on,
that this includes side effects like ad hoc debugging `println`s.[^1]
For example if you have:

```clj
(let [coll (map (fn [x]
                  ;; 0: some bug that will throw an exception
                  )
                coll)
      _ (println "Everything AOK thus far -- or maybe not!")]
  ;; 1: use coll in way that forces the lazy seq
  )
```

The error won't occur at 0, it will only occur at 1. The progress
`println` will be misleading. At least it misled me, for awhile, about
the actual location of a bug.

[`racket/stream`]: http://docs.racket-lang.org/reference/streams.html

With Clojure and Haskell, I'll need to keep in mind when and where
laziness is used as the default policy. If I understand correctly, in
Clojure that means lazy sequences, and in Haskell lazy evaluation
generally.

[^1]: Are `println`s a sophisticated debugging technique? Nope.
But some experienced programmers use them as a quick first resort (even
when they're willing and able to fire up a real debugger).

# Load vs. modules

After about a week hands-on with Clojure one of the things I miss the
most from Racket is modules. Not even Racket's submodules. Just plain
modules.

Clojure namespaces handle name collisions. But modules go further:

1. Forward references are OK.

2. Redefinitions are flagged as errors.

3. Deleted definitions actually disappear from the environment on
re-eval of the source file.

In other words, Clojure seems to be like Racket's `#lang racket/load`,
which isn't recommended for general use.

An example scenario: I rename a function from `foo` to `bar`. I
overlook updating a call site. Clojure doesn't warn me with an error.
Worse, I change `bar`'s behavior. But old `foo` still exists -- and is
being used at the overlooked call site. Hilarity and gnashing of teeth
ensues.

This isn't a hypothetical example. It's happened to me a couple times
in not that many days of hands-on with Clojure. On the one hand, this
seems like an insane programming workflow. On the other hand, I have
already learned to "measure twice, cut once" when renaming -- and to
bite the bullet and invest 10 seconds in a cider-restart. So if life
had to be this way, I could cope.

But why does it have to be this way? I actually started to draft a
[`module`][clojure-module] macro for Clojure. As I put in its README:

> **DISCLAIMER**: This is a first draft by someone who...
> 
> - has been hands-on with Clojure for just a week
> 
> - doesn't necessarily appreciate how Clojure namespaces work
> 
> - doesn't know the complete story behind Racket modules

I can imagine Inigo Montoya telling me, "I do not think `module` means
what you seem to think it means". Yeah. That's probably me, with
[this code][clojure-module]. At the moment it's an exercise in
starting to think about what might be involved.

[clojure-module]: https://github.com/greghendershott/clojure-module

# Conclusions and next steps

There is a lot about Clojure that I really, really like and enjoy. At
times I do wish it had better tooling and were built on a
more-rigorous foundation.

I need to determine what to do next -- spend more time with Clojure,
or move on to Haskell. Also I need and want to spend significantly
more time pairing with people here -- which will at least partially
entail working with a variety of other languages and platforms.

So it's likely that I'll take at least a brief break from Clojure. But
I'll return at some point.
