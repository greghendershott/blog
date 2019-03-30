    Title: Planet vs. the new package system
    Date: 2013-04-11T13:58:19
    Tags: Racket

Recently I've shared some new libraries using the new package manager,
but _not_ uploaded them to Planet.

What about my existing Planet libraries? Yesterday Danny Yoo pointed
out a bug in the Dynamo module of my Amazon AWS library on Planet.

The bug wasn't present in my GitHub repo--I'd neglected to go through
the steps of making a new version and uploading to Planet.

By contrast, with the new package manger, it would have been
automatically up-to-date.

<!-- more -->

So I did two things:

1. Updated the `aws` and `http` packages on Planet to fix that bug.

2. Decided to use the new package manager instead, going forward.

I suppose I'll update a Planet library if there's some really ugly
bug. But when adding functionality to existing libraries, or for new
ones, I plan to use _only_ the package manager. I'm not enthusiastic
about juggling both.

## The new package manager

I really like the new package manager, because overall it's a
"lighter" and faster experience. I like the way it delegates to other
systems such as GitHub (or whatever you prefer).

My only two quibbles aren't very serious:

1. Making subdirs. A "package" is "one or more collections". The "or
   more" part means you need to move the collection into a subdir. So
   most of what you had in `project` gets moved into a new subdir,
   `project/project`. (What's left up top is basically just the
   `README.md`, your `.gitignore`, and an `info.rkt` for the package
   manager.) Although this is a small nuisance (and so is doing stuff
   like `cd ../../foo/foo` at the command line) it's not _that_ big a
   deal in my opinion.

2. One thing I like about Planet it that it hosts the Scribble
   documentation for a library, if any. Reading the doc for a lib--or
   indeed seeing if it has any doc at all--helps me decide how much to
   rely on a lib. The new package manager doesn't host docs like
   Planet, because it doesn't host files--it points to something like
   GitHub which has the files. However, you can do
   
   `scribble --markdown manual.scrbl`
   
   in the latest version of Racket to generate a Markdown format file.
   GitHub will display that `manual.md` file nicely. There's your
   hosted doc. With some caveats: It's one file, which may be unwieldy
   for huge libraries. You don't get the extensive hyperlinking you
   get in Scribble HTML output. Even so, it conveniently addresses the
   question, how to host documentation.[^1]

## Going forward

The status quo situation with Planet and the new package manager is
fine. Technically the package manager is still in beta.

At some point, the existence of both will probably become a
liability. Eventually it would be more beneficial to have a simple,
clear message for folks who want to contribute as well as use
libraries.

---

[^1]: Of course you could also generate the usual HTML files and host
them using GitHub Pages for your project. Personally, I find it awward
to juggle the special `gh-pages` branch. But it might be worth it for
some projects.
