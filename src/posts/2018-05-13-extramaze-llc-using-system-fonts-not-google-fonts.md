    Title: Extramaze LLC: Using system fonts (not Google fonts)
    Date: 2018-05-20T00:00:00
    Tags: Racket, Extramaze

In my [previous post] I discussed what I'm doing with
[deals.extramaze.com] -- and what I'm intentionally _not_ doing. Since
then, I'm not-doing more. This improves performance and simplifies the
content security policy.

[previous post]: /2018/05/extramaze-llc-using-racket-postgresql-aws-but-no-ads-or-js.html
[deals.extramaze.com]: https://deals.extramaze.com

<!-- more -->

In the [privacy] section I described various third-party services the site is
_not_ using. On [Recurse Center's] private chat ([Zulip]), [Josh Bronson]
pointed out that using Google-hosted fonts might not be the best idea:

[privacy]: /2018/05/extramaze-llc-using-racket-postgresql-aws-but-no-ads-or-js.html#trying-to-do-the-right-thing-privacy

[Josh Bronson]: https://twitter.com/jab_______

[Recurse Center's]: https://www.recurse.com/

[Zulip]: https://zulipchat.com

- Privacy: Using `fonts.googleapis.com` tells Google what sites the
  user is visiting. It doesn't make much sense to avoid Google
  Analytics but keep using Google Fonts.

- Performance: Downloading custom fonts takes time. This can be
  especially annoying on slower mobile connections.

Often these are two sides of the same coin: Leaking information to a
third party usually consumes extra time and bandwidth to make extra
connections and transfer data.

Instead host the font files locally? That would address privacy. But
it could make performance worse not better. Using fonts hosted by
Google _does_ have the advantage that the user might already have
downloaded them, as a result of some other web site using them.

Josh asked if I'd considered using system fonts, and shared
[two](https://furbo.org/2018/03/28/system-fonts-in-css/)
[links](http://markdotto.com/2018/02/07/github-system-fonts/).
Initially I was reluctant -- but but but... my carefully chosen fonts!
And yes, it changed the look and feel of the site, somewhat. But after
living with it for about half an hour, I thought it was just fine.

# Accessibility

At the same time I was looking at fonts, I'd been starting to review
accessibility issues. Such as making sure that:

- `img` elements have `alt` attributes
- `svg` elements have child `title` elements
- the `html` element has a `lang=en` attribute
- all `id` attributes on a page are unique

Another item on this list: Ensuring that font colors have sufficient
contrast. So I did that while I had the hood open.

(I have more work to do for accessibility: I've only just started to
use the site with the macOS screen reader. Maybe this should be
another blog post.)

# Content Security Policy

Dropping Google hosted fonts let me simplify the content security
policy. Indeed it seems to have cut down on the number of violation
reports.

Since my last blog post, I had temporarily switched from
`Content-Security-Policy` to `Content-Security-Policy-Report-Only`. I
was nervous because I didn't understand all the violations.
[report-uri.com] helps by filtering things like violations caused by
browser extensions. But even after such filtering, I had violations
that made no sense to me.

[report-uri.com]: https://report-uri.com

Using system fonts, that situation improved significantly. I feel good
about cranking it back up again to be enforced instead of
report-only.[^footnote-about-using-both]

[^footnote-about-using-both]: Of course you can use both headers. `Content-Security-Policy` is what you're enforcing, and `Content-Security-Policy-Report-Only` can be used to dry-run changes.

I suppose this illustrates a concern using any third-party service.
Effectively it mutates your web site with various scripts or fonts or
styles. What are all its mutations? You may think you know from
observation: Oh it needs a script from a certain URI. But oops, in a
certain scenario it turns out that it also needs a style from another
URI. Surprise. And that's just today. What if the service changes in
the future?

Really, every service ought to state exactly what content security
policy settings it needs to work.
