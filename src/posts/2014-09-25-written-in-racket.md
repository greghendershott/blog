    Title: Written in Racket
    Date: 2014-09-25T10:14:05
    Tags: Racket

This is an overview of things I've created using Racket. Two
motivations for writing this now:

1. Over the last week I was at three conferences (whew!) where, when
   meeting or catching up with someone, I had to explain what I've
   been doing. I mentioned my current projects or projects I guessed
   they'd relate to. But that's not necessarily representative of all
   that I've been able to do with Racket. I wish I'd been able to give
   a better overview. I have quite a few repos on GitHub, but that's
   just a big list with no structure.

2. In about a week I start my batch at [Hacker School]. I'll likely
   spend less time with Racket, because the whole point is to learn
   new things. Now is a good time to take inventory. And I'll be
   better prepared to talk about Racket there.

[Hacker School]: https://www.hackerschool.com/

As a result, here's an inventory, grouped into categories.

<!-- more -->

# Web services

I wrote a [wrapper for most of the Amazon AWS web services][aws].
Working with AWS directly at the HTTP level is really fun and
educational. Well. It's fun when writing a wrapper library. Not so fun
when writing an app. Hence a wrapper library.

To support [aws], I made a small [http] library to help with things
like `100-continue` responses, chunked transfer decoding, and so on.

I made an [interface to Google API Discovery Service][gapi]. This was
fun because I didn't hand write wrapper code. Instead, I query this
Google web service that describes their web services. Racket macros
use that response data to create wrapper functions. I even generate
Scribble documentation.

More generally, [webapi-markdown] is a way I came up with to do
"literate" (in the sense of "literate programming") web API
specifications. A web service is both documented and specified using a
markdown file containing parameterized HTTP request and response
templates. [wffi] (think "web foreign function interface") is an
implementation for Racket.

When I was grumpy with Google for killing Google Reader, I wrote
[feeds2gmail], which is similar to the venerable rss2email, but:

- Creates emails directly in your mailbox using IMAP `APPEND`
  (faster and no delivery issues).

- Uses two IMAP mailboxes -- Gmail "labels" -- to be a bit closer to
  the GReader experience.

[aws]: https://github.com/greghendershott/aws
[http]: https://github.com/greghendershott/http
[gapi]: https://github.com/greghendershott/gapi
[webapi-markdown]: https://github.com/greghendershott/webapi-markdown
[wffi]: https://github.com/greghendershott/wffi
[feeds2gmail]: https://github.com/greghendershott/feeds2gmail

# Static blogging and markdown parsing

My most-starred project on GitHub is [Frog]. Making a static blog
generator isn't very special. But Frog is special among my Racket
projects because it's an application, not a library or programming
tool. I wanted something that was much easier and simpler to install
and use than Octopress. To the extent that succeeded, it's largely due
to the choice of using Racket.

As I [blogged about][markdown], the hardest part of that project was
parsing markdown. I ended up using a monadic parser combinator
approach, very similar to how Pandoc does this. (Later I used the same
approach to whip up a [toml] parser.)

[Frog]: https://github.com/greghendershott/frog
[markdown]: http://www.greghendershott.com/2013/11/markdown-parser-redesign.html
[toml]: https://github.com/greghendershott/toml

# A racket mode for Emacs

Although a big chunk of [racket-mode] is obviously Emacs Elisp, most
of the more challenging and interesting bits are actually written in
Racket. Such as:

- Given a symbol, how do you find the definition site? How do you
  extract its "signature" and/or contract and/or Typed Racket type?
  How do you find its installed documentation, if any?

- How do you let people write programs using `racket/gui`, but not
  make everyone pay the cost of loading it and popping up a `racket`
  frame window all the time? In other words, how do you do an
  on-demand, one-time initializiation of `racket/gui`?

- And so on.

I hope I have time to blog more about this.

[racket-mode]: https://github.com/greghendershott/aws

# Some Clojure idioms for Racket

I have a few "armchair" languages. I've read a lot about them, read
quite a bit of code, but not yet written much myself. At the moment
these include Haskell and Clojure. During my time at Hacker School I
expect to move these out of the armchair category.

Meanwhile, I learned enough about Clojure to like a few of its ideas
or idioms and want to use them in Racket. The result, with
contributions from some other people, is [rackjure].

A few features -- such as applicative dictionaries, curly brace
dictonary literals, and function literals -- must be used as a Racket
language: `#lang rackjure`. The remaining features can be `require`d
as needed.

[rackjure]: https://github.com/greghendershott/rackjure

# Things not on GitHub

I did a few things that I didn't put on GitHub. Yeah, I know. Code or
it didn't happen. Regardless....

## Signal processing

I experimented with signal processing combinators (including but not
limited to audio signal processing).

Why isn't this on GitHub?

1. It's just proof-of-concept, playing with signals being functions
   from time to a value. (Pleasant surprise: This turned out to be
   much more performant for audio processing than I expected.)

2. Eventually I realized I was starting to reinvent aspects of FRP;
   maybe I should back up and learn from others.

3. It was a chance for me to try Typed Racket. With great results:
   Typed Racket made my code less buggy and faster, both. I could
   specify specific vs. generic signals -- e.g. `(Signal Float)` vs.
   `(Signal a)`.

4. My background is the music tech industry. I'm leery of people's
   expectations. ("Isn't it cute, the monkey thinks it can write
   professional audio DSP code.")

## Web app middleware framework

"Lob" is a low-level library for web applications, in the spirit of
Python's WSGI, Ruby's Rack, and Clojure's Ring. Like those, Lob is
intended to be the glue between a web server and a higher-level web
application framework.

Although I was pretty happy with how this turned out, I wasn't using
it actively in a real world project. It's one thing to dogfood a
project and support other people using it for similar use cases. But
it's another thing to, well, not do that. So... not on GitHub.

# Writing about Racket

I wrote [Fear of Macros], which is a sort of autobiographical tutorial
for Racket macros. Although it's probably not the best introduction
for everyone, it might help people like me.[^macros]

[^macros]: Macros might be one of those subjects -- like continuations
or monads? -- where people need to read a few introductions until one
"clicks" for them. I suppose the common theme among these things is
that they tend to get mythologized and jargonized. Some of us need to
take a deep breath, see how the underlying mechanism is actually quite
simple, and build up from there.

I've written [some blog posts](http://www.greghendershott.com/tags/Racket.html).

I've tried to pull my weight on the Racket user mailing list and on
Stack Overflow -- it's fun as you gradually gain confidence to help
others the way you were helped. I find it's also a great test whether
I really understand something as well as I imagined.

[Fear of Macros]: http://www.greghendershott.com/fear-of-macros/

---

# All other

I think that sums up nearly everything I've made into a repo on
GitHub, or even an actual Racket package for others to use.

Of course I've tried many more things in Racket, that I have sitting
around here and there. Web crawling and scraping. Alternative
definition forms. And more. All have been fun to do, and most have
taught me more about Racket and/or programming in general.

---

