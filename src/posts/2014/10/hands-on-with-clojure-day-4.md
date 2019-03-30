    Title: Hands-on with Clojure day 4
    Date: 2014-10-14T17:52:11
    Tags: Clojure, Hacker School

Overview:

- Spent time with [Clojure Cookbook].
- Started a [cheat sheet] for Racket ↔ Clojure.
- Looked at `split-with`.
- Started a port of [wffi] from Racket to Clojure.
- Confusion: Aggregates and generics.

[Clojure Cookbook]: https://github.com/clojure-cookbook/clojure-cookbook
[cheat sheet]: https://github.com/greghendershott/racket-clojure-cheat-sheet
[wffi]: https://github.com/greghendershott/wffi

<!-- more -->

# Clojure Cookbook

I discovered [Clojure Cookbook], which describes itself as:

> *Clojure Cookbook* doesn't just teach you Clojure, it also shows you
> how to use the language and many of its common libraries. The most
> difficult part of mastering any language is knowing how to apply it,
> in an idiomatic way, to tasks that real software developers
> encounter every day. This is especially true of Clojure.
>
> With code recipes that teach you how to use the language in a
> variety of domains, *Clojure Cookbook* goes beyond simply teaching
> Clojure syntax and semantics. It contains annotated example code
> with detailed analysis and explanation for hundreds of real
> programming tasks. You can read the book straight through to gain
> insights about Clojure, or use it as a reference to solve particular
> problems.


You can `git clone` the [Clojure Cookbook] project and work with it in
Emacs. This is a nice experience.

You can read each section in an `adoc-mode` buffer, with cider-repl
active. That way you can <kbd>C-x C-e</kbd> to see the result on the
Emacs status bar (or if you prefer, paste or type directly in the the
cider-repl buffer).

Plus you can bind a key like <kbd>M-+</kbd> to an Elisp function that
finds and advances to the next section.

I found most sections to be helpful. Not only do they show how to do
X, they get into the rationale of when and why should use a certain
Clojure approach vs. another. Very good stuff.

# Racket ↔ Clojure Cheat Sheet

As I've learned Clojure equivalents for Racket functions, I've jotted
them down in notes. The notes got long enough that I moved them into
org-mode tables -- resulting in this [cheat sheet].

The first part covers, given Racket function X, what's the exact or
close equivalent Clojure function Y? The second part is for the other
direction, from Clojure to Racket.

Again, these are my rough working notes. Don't rely on it. I wouldn't
be shocked if some of it is subtly -- or blatantly -- wrong.

I found that, as is often the case with taking notes, the act of
taking them makes it less-likely to need to refer to them.

# Cheat sheets, `split-with`

Speaking of cheat sheets, on `#clojure` someone reminded me of the
[Clojure cheatsheet]. This seems like something to keep open in a
browser tab. Or, say, print and paste on the inside of my forehead.

[Clojure cheatsheet]: http://clojure.org/cheatsheet

I was on IRC asking if Clojure had some equivalent to Racket's
[`splitf-at`], with an implementation faster than the conceptual one I
was able to come up with:

[`splitf-at`]: http://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._splitf-at%29%29

```clojure
(defn split
  "FIXME: This is the conceptual, inefficient implementation. Should
  re-implement like Racket's splitf-at."
  [pred coll]
  [(take-while pred coll)
   (drop-while pred coll)])
```

Someone pointed out [`split-with`], which I could have found on the
cheat sheet.

[`split-with`]: https://github.com/clojure/clojure/blob/clojure-1.6.0/src/clj/clojure/core.clj#L2698

So that's cool. However <kbd>M-.</kbd> on [`split-with`] shows an
implementation that is... the conceptual one. I guess that could be a
micro-project -- write an implementation that doesn't excessively
traverse and create intermediate collections.

# wffi for Clojure

A couple years ago, I was reading so much Amazon Web Services
documentation that included what were essentially HTTP request and
response templates. Such as the following, albeit much more
complicated than this:

```
GET /users/{user}/?key={val}
Header: {value}
  ...
```

Eventually it dawned on me that it would awfully be nice if web
service documentation like this could actually drive code generation.
Both generate client wrappers to make requests to the service, and,
generate API glue for the service's server. That was the idea behind
[webapi-markdown]. For example, you could take some service's
documentation, run it through a tool, and voila, an "SDK" in your
favorite language.

[webapi-markdown]: https://github.com/greghendershott/webapi-markdown

So [webapi-markdown] is just a spec. The one implementation to-date is
in Racket: [wffi]. (The name implying both "web FFI" and "stinky".)

In searching for a somewhat more challenging project to do in Clojure,
I figured this might fit the bill.

So I spent the last day working on it. It's been slow going, but so
far I've had a chance to learn:

- How to use [markdown-clj] for parsing markdown. Although it returns
  an HTML string, which isn't ideal, it's easy enough to convert that
  using `tagsoup` into structured data. (I keep thinking of these as
  "Clojure's equivalent of x-expressions". Is this known as "hiccup"?
  Not sure.)

- How to use [Instaparse]. I only got a couple hours into this, but so
  far it seems pretty spiffy. It was fun to see the acknowledgment in
  the README that Danny Yoo's [Ragg] was the inspiration. It is
  definitely nice to use in the same way that Ragg is, and more.

And eventually this project will get into areas like:

- Code-generation via Clojure macros.

- Generating docs (via doc strings, I suppose).

- Distributing a project on Clojars (if I take it that far).

So I think that's a pretty good chunk of real-world stuff, which makes
it worth pursuing this, even if there's no value in releasing it per
se.

The code is still pretty raw, and my grammar for Instaparse needs
work. So I'm making commits, but not confident enough to push stuff to
GitHub quite yet.

[markdown-clj]: https://github.com/yogthos/markdown-clj
[instaparse]: https://github.com/Engelberg/instaparse
[Ragg]: https://www.hashcollision.org/ragg/

# Aggregates and generics

One area I'm still confused by is Clojure's variety of approaches to
aggregate data types and polymorphism.

Coming from Racket, we have structs and generics. `struct`s have
compile-time defined fields. As a result:

- They're fast: A field is a direct offset.
- They're strict:
    - You can't accidentally add or omit a field.
    - Structs can inherit fields from other structs, strictly.

Of course we also have dictionaries (hash tables and association
lists) when we want runtime flexibility.

My current impression is that Clojurians would often use a `map`
instead of a `struct` in many situations. I think?

Clojure does have things that seem to be in the same ballpark as
`struct`s. `defrecord` sounds like it is about `struct`s. Is it sugar
for maps? `deftype` is, erm... I don't know. Clojure Cookbook has a
quote from Chas Emerick:

> Is your class modeling a domain value--thus benefiting from hash
> map-like functionality and semantics? Use `defrecord`.
>
> Do you need to define mutable fields? Use `deftype`.

Hmm. That's not what I would have guessed from the name, "type". Plus,
I thought mutability was strongly discouraged in Clojure.

The docs for `defrecord` and `deftype` seem a bit terse and opaque to
me: The words make sense but aren't crisply connecting to concepts I
already know.

So, clearly I need to learn more in this space.

# Requires are less often, but still, tricky

Although it's much better, I still seem to get confused by requires.
Good example is clj-tagsoup. I kept trying to `:require`
`pl.danieljanus.tagsoup.core`. But it's just `pl.danieljanus.tagsoup`.
The classpath error message isn't very helpful for me. If I go in
`~/.m2` and look, I don't see source files. I need to go up on GitHub
and view the source. That's how I eventually noticed it's
`src/pl/danieljanus/tagsoup.clj`. Not e.g.
`src/pl/danieljanus/tagsoup/core.clj`.

To be fair, I suppose this is really about one library's choice. And
I'm new to Clojure. But I don't seem to struggle with this in Racket.
Especially when an installed package has Scribble-generated docs, the
docs automatically show a `(require foo)` that, well, just works the
first time.

# Reflection and next steps

Notwithstanding my whinge in the previous section, I'm hitting tooling
and cider speed bumps much less frequently. I felt like yesterday was
the first time I could actually focus on the code, without
distraction, for solid hours at a time. Although I still don't feel
like I've figured out an optimal Clojure workflow, it is easier for me
to imagine such a thing being possible someday.

Next up, I plan to keep working on the wffi port, and see how far I
can take that this remainder of this week.
