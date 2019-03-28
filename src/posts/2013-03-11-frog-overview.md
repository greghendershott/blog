    Title: Frog overview
    Date: 2013-03-11T15:37:00
    Tags: blog, Frog, Racket, software

> UPDATE: This was an early post. For up-to-date info, see
> [Frog][].

When the code settles down a bit I'll put it in a GitHub repo, and
write a full README.md. Meanwhile, here's an overview.

<!-- more -->

## Overview

Frog is a static web site generator written in [Racket][]. You generate
files. To deploy, you push them to a GitHub Pages repo, or copy them
to Amazon S3, or whatever.

You write content in Markdown.

Posts get a variety of automatic blog features.

You can also create non-post pages.

Yes, it's very much like Octopress and countless others. But it
doesn't require any Ruby gemmage. The only non-Racket part is
optionally using [Pygments][] to do syntax highlighting.

## Layout

The layout is basically this:

```
project/
  _src/
    footer.md
    About.md
    ...
    posts/
      2013-03-07-a-blog-post-title.md
      2013-03-11-another-blog-post-title.md
      ...
index.html
About.html
sitemap.txt
tags/
feeds/
css/
js/
...
```

## Posts

You create new posts in `_src/posts`. They should be named
`YYYY-MM-DD-TITLE.md`. You can do `racket frog.rkt -n "My Title"` to
create such a file easily. This will also fill in the required
meta-data section. The markdown file starts with a code block
(indented 4 spaces) that must contain these three lines:

```
    Title: A blog post
    Date: 2012-01-01T00:00:00
    Tags: foo, bar, tag with spaces, baz

Everything from here to the end is your post's contents.

If you put `<--! more -->` on a line, that is the "above-the-fold"
marker. Contents above the line are the "summary" for index pages and
Atom feeds.

<!-- more -->

Contents below `<!-- more -->` are omitted from index pages and Atom
feeds. A "Continue reading..." link is provided instead.

```

`Title` can be anything.

`Date` must be an ISO-8601 datetime string: `yyyy-mm-ddThr:mn:sc`.

`Tags` may be blank (although you have to include the `Tags:` part).

### Automatic post features

As you can see from this blog post, an "On this page"
table-of-contents is automatically generated if there are any section
headings in your posts.

Posts are automatically included in index pages and feeds.

`/index.html` is an index for all posts, listed newest first.

`/feeds/all.xml` is an Atom feed for all posts.

For each tag, there is a `tags/<tag-name>.html` index page (also
listed newest first) and a `/feeds/<tag-name>.xml` Atom feed.

### The `DRAFT` tag

The tag `DRAFT` (all upppercase) causes the post _not_ to be generated.

This way, you can commit the source `.md` file to your repo, and push,
but there will be no corresponding `.html` generated and pushed.  (The
use case here is GitHub pages. If you deploy to something like Amazon
S3, the similar point is that no `.html` file will be generated and
deployed to that.)  _I should rewrite this to be more clear about
different usage scenarios_.

## Non-post pages

You can put other `.md` files in `_src`, and in any subdirs of
it. They will be converted to HTML as non-post pages.  For example,
`_src/About.md` will be `/About.html` in the site.

Non-post pages are _not_ included in any automatically generated index
pages or feeds.  If you want them to be linked in, you must do so
manually.

> Note: The navbar is currently hardcoded to look for `/About.html`, and
> that's it. It's a to-do item to let you specify more items, perhaps
> using a `_src/navbar.md` file.


## footer.md

The special file `_src/footer.md` is converted to HTML and placed at
the foot of all pages (both posts and non-post pages).


## sitemap.txt

A `/sitemap.txt` file (for web crawlers) is automatically generated
and includes all post and non-post pages. (It does _not_ include index
pages for tags.)

## Sharing buttons

Sharing buttons for Twitter and Google+ are automatically put at the
bottom of posts and non-post pages.

## Code blocks

Frog optionally uses [Pygments][] to do syntax
highlighting. In your markdown using backtick code blocks you can
specify a language:

    ```language
    some lines
    of code
    ```

That _language_ is given to Pygments as the lexer to use.

For example this:

    ```js
    /**
     * Some JavaScript
     */
    function foo()
    {
        if (counter <= 10)
            return;
        // it works!
    ```

Yields this:

```js
/**
 * Some JavaScript
 */
function foo()
{
    if (counter <= 10)
        return;
    // it works!
```

And this:

    ```racket
    #lang racket
    ;; Finds Racket sources in all subdirs
    (for ([path (in-directory)])
      (when (regexp-match? #rx"[.]rkt$" path)
        (printf "source file: ~a\n" path)))
    ```

Yields this:

```racket
#lang racket
;; Finds Racket sources in all subdirs
(for ([path (in-directory)])
  (when (regexp-match? #rx"[.]rkt$" path)
    (printf "source file: ~a\n" path)))
```

I have a soft spot for [Pygments][] because it's actually the first
existing open source project to which I contributed. I added a lexer
for the [Racket][] language. More importantly it has lexers for tons
of languages and is used by things like GitHub, BitBucket, and so
on. Plus, it fits the spirit of static web site generation better than
JavaScript options like [SyntaxHighlighter][].

## To-Do

What's left for a "1.0" (or maybe a "Beta 1"):

- Some per-project data (such as the author, title, path to Pygments,
  etc.) are currently Racket parameters. I need to add code to read
  those from a configuration file.

- I need to document the command-line interface.

- The command-line interface has a "preview" flag to start a local web
  server. I'm using something from another project that's not baked
  and ready for release. I need to redo this to use the built-in
  Racket web server.

Some things for "1.1" (or "Beta 2"):

- Paginate the index pages (show only N posts at a time, with
  older/newer links).
  
- Get the Bootstrap responsive mode working. It was working for my
  previous hand-coded site, but for some reason it's not working for
  Frog.
  
- Let the user supply a navbar.md to populate the top nav
  bar. Probably as a Markdown unordered (bullet) list of links.


[Racket]: www.racket-lang.org
[Pygments]: http://pygments.org/
[SyntaxHighlighter]: http://alexgorbatchev.com/SyntaxHighlighter/
[Frog]: https://github.com/greghendershott/frog
