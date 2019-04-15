    Title: Exploding Frog
    Date: 2019-04-03T00:00:00Z
    Tags: Racket, Frog

I'm writing and publishing this post using something other than [Frog].

Having said that, I'm not planning to abandon maintaining Frog.

[Frog]: https://github.com/greghendershott/frog

<!-- more -->

Over the past week I explored doing something I've wanted to try for
many years: "Exploding" Frog from an app that you run, into [a set of
little commands that you call from a
Makefile](https://github.com/greghendershott/blog).

I believe there's a variation of [Greenspun's Tenth Rule]:

> Any sufficiently complicated static blog generator contains an
> ad-hoc, informally-specified, bug-ridden, slow implementation of
> half of `make`.
>
> -- Someone who has made a static blog generator

[Greenspun's Tenth Rule]: https://en.wikipedia.org/wiki/Greenspun's_tenth_rule

If you look at Frog's code, by volume, an awful lot of it consists of:

1. Figuring out what needs to be (re)built.
2. "Path math".
3. Configuration, customization, templates.
4. Support for Bootstrap or various surveillance capitalism gadgets.

On the one hand, it is certainly very "Rackety" to write this in
Racket. On the other hand, Racket advocates language-oriented
programming. Make is a language for expressing the first two things.

As for the third item: Originally, I thought it would be cool to write
Frog as an app, thinking people might use it even if they weren't
particularly into Racket programming. In reality, of course
approximately 103.8% of Frog users like to program in Racket. So
maybe, call this crazy, programmers who want a blog generating program
to work a little differently could, I don't know, change the program a
little. Maybe there need not be layers of configuration and
customization.

---

And yet, I've never really known that much about GNU Make. Just enough
to cargo cult. And the GNU Make documentation, although thorough,
never really "clicked" for me. This time, though, I stuck with it. It
helped to have a non-trivial project in mind.

The most confusing thing for me at first, was, Make seems oriented
around defining targets and "pulling" those from sources. "This
executable depends on linking these object files. These object files
depend on these .c and .h files." I didn't really grok how to make it
be "push"-driven: We have a bunch of (say) `.md` post sources. Yes
those get built into `.html` files, but we also have tags, and feeds,
and...?

In the end I made use of some variables to define the sources,
intermediate build files, and final outputs:

```make
# Configure where are sources, build cache, and root of the web site
# output.
src   := src
cache := .cache
www   := www

# Make normally "pulls" targets from sources, but we want to "push"
# sources to targets. As a result, we need to build lists of sources
# and from those build lists of targets.

post-sources := $(shell find $(src)/posts -type f)
post-caches  := $(patsubst $(src)/posts/%.md,$(cache)/%.rktd,$(post-sources))
post-htmls   := $(patsubst $(cache)/%.rktd,$(www)/%.html,$(post-caches))

tag-caches     := $(wildcard $(cache)/tags/*)
tag-htmls      := $(patsubst %,$(www)/tags/%.html,$(notdir $(tag-caches)))
tag-atom-feeds := $(patsubst %,$(www)/feeds/%.atom.xml,$(notdir $(tag-caches)))
tag-rss-feeds  := $(patsubst %,$(www)/feeds/%.rss.xml,$(notdir $(tag-caches)))

non-post-sources := $(wildcard $(src)/non-posts/*.md)
non-post-htmls   := $(patsubst %.md,$(www)/%.html,$(notdir $(non-post-sources)))
```

Then, for example, a rule like this is how we turn a `.md` source into
an intermediate `.rktd` cached file:


```make
$(cache)/%.rktd: $(src)/posts/%.md
	$(make-post-cache) $< $(abspath $(cache)/tags) $@
```

and that into an `.html` file:

```make
$(www)/%.html: $(cache)/%.rktd rkt/page-xexpr.rkt
	$(make-post-html) $< $(www) $@
```

What is this business about an "intermediate post cached file"? Well,
the overall model is a two-stage "compile and link" sort of metaphor,
similar to Frog. Post `.md` sources only define the "body" of a post
page, e.g. the `<article>` element that will go somewhere in a page.
Building that can be semi-expensive when you do enhancements like
syntax-highlighting and automatic links to Racket documentation. It's
nice not to need to redo that, simply because some other element on
the containing page changes. Also, each post has one or more tags, so
the first pass is a chance to build an index of tags to posts, which
the second pass can use to make feed files and an index page for each
tag.

The little commands that the `Makefile` calls are defined in in a
`rkt/` subdirectory. Some of them `require` modules from Frog. Others
are copypasta with some adaptation. Any of these files that could be
truly generic -- i.e. won't elicit a pull-request someday to get
different blog behavior -- I'd like to move eventually into a
"tadpole" package. The remaining code should go in some repo -- _not_
a package -- as example code. That is, you could copy it -- probably
not even fork it -- and just hack away. It is your blog, so it is your
program.

So far I'm enjoying this new approach.

Again, I still plan to maintain Frog, in terms of fixing bugs and
keeping it working on newer versions of Racket. Already in the past
couple years I've been super reluctant to add new features, and
haven't. All I'm saying here is that I plan to continue that
not-doing.
