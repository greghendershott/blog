    Title: Hands-on with Clojure day 3
    Date: 2014-10-10T11:13:34
    Tags: Clojure, Hacker School

Please see the usual disclaimers from my [previous][] [posts][].

[previous]: http://www.greghendershott.com/2014/10/hands-on-with-clojure.html
[posts]: http://www.greghendershott.com/2014/10/hands-on-with-clojure-day-2.html

As I mentioned yesterday, my next toy project is to write wrappers
libraries for the new [Hacker News API]. This seems like a good
exercise because the REST API is very simple, and I have experience
doing this sort of thing in Racket. In fact, I'll do the same thing in
both Racket and Clojure.

[Hacker News API]: https://github.com/HackerNews/API

The result is [clacker-news] and [racker-news]. Trademark registration
application is in-process.[^1]

[clacker-news]: https://github.com/greghendershott/clacker-news
[racker-news]: https://github.com/greghendershott/racker-news

[^1]: Kidding.

<!-- more -->

Although I knew this would be a simple project, it turned out to be
simpler than I expected:

- The Hacker News API is just a half-dozen items, so that helps.

- Both Racket and Clojure have easy ways to parse JSON into the
  relevant idiomatic representations. (As you might imagine, Racket
  is more `list`y and Clojure is more `vector`y, but they are pretty
  similar. Because JSON.)

- I didn't try to do anything fancy with error-handling. If there's an
  HTTP-related exception, it's going to bubble up to the user of my
  library. I think this is probably fine.

- I didn't try to do anything fancy with retries. This is probably
  less-fine. Given response like say `429 Too Many Requests`, it would
  nice if my library would automatically retry some number of times
  (with an exponential delay back-off). But it doesn't.

However, maybe it was unduly easy -- because I overlooked something
important in Clojure (or about making REST API requests in general).
If so, feel free to hit the comments.

# Redefinitions and the top-level

While doing some copy-pasta of the Racket source to Clojure, I
accidentally ended up with two `defn`s of the same function,
`get-user`. The thing is, Clojure did _not_ give me an error message.
This really surprised me. In Racket, this would be a redefinition
error.

I guess this is related to my observation in yesterday's post about
the need to use `declare` for forward references. If I understand
correctly, this means that Clojure's evaluation model is closer to a
simple `load`: It is essentially equivalent to typing stuff at a
top-level REPL prompt. Things are evaluated one s-expression `read` at
a time.

So for example it's perfectly fine to redefine something at the
top-level in the REPL.

And, if you `read` things one s-expression at a time -- as opposed to
an entire file and/or namespace -- you can't know about something that
isn't defined yet. You need a hint like `declare`: "Chill, I'm going
to supply it later."

What I'm used to from Racket is an actual module system. As I
understand it, the first unit of evaluation is a module. And the
`#lang` business is a shorthand for `module` forms.  In other words:

```racket
#lang racket
(define (id x)
  x)
```

is shorthand for:

```racket
(module racket
  (define (id x)
    x))
```

And in fact you will see older Racket files that use the `module` form
explicitly like that (as well as newer `#lang` files that use `module`
forms within, i.e. for nested modules).

Maybe I'm misunderstanding, and the business about modules is
orthogonal to the business about redefinitions and forward references.
Maybe that's really about `read`-ing s-expressions one at a time like
in a top-level REPL. In any case, I don't love this aspect of Clojure.[^2]

[^2]: Among Racketeers, a famous quote is, ["the top-level is hopeless"](https://gist.github.com/samth/3083053).

# Next steps

I need an idea for a gradually more-complicated project to try, next.

Ideally it would exercise something special/strong about Clojure, such
as the persistent immutable data structures, concurrency primitives,
or so on.

(As a counter-example, I could explore macros in Clojure, but my
expectation is that's an area that might be disappointing compared to
Racket. Instead I'd like to find something where I'm more likely to
say, "Dang, I wish Racket did this.")

If you have suggestions, let me know in the comments, and thanks in
advance.
