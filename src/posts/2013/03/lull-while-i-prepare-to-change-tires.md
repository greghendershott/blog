    Title: Lull while I prepare to change tires
    Date: 2013-03-08T16:27:00
    Tags: software, Racket, blog, Frog

I'd been trying to stick to a roughly Tuesday and Thursday schedule
for posting here.

I haven't this week because I've been trying to work up a replacement
for using Blogger.

Basically, I want to write posts in simple Markdown, and generate the
blog statically. To be hosted on GitHub or S3 or whatever. And I want
it to use Bootstrap so I don't have to reinvent that wheel.

<!-- more -->

I realize a bunch of tools purport to do this. The ones I've looked at
so far, such as Octopress, require installing a bunch of Ruby
hoo-hah. I'm a a Racket guy. And I recently wrote a Markdown parser in
Racket (with something else in mind, not this project).

As a result, I've been spending the last couple days working on the
static site generator.

My working name for it is Frog. As in <b>fr</b>ozen bl<b>og</b>.

The gist of it has some together pretty quickly in Racket, which is a
joy to use.

The bulk of the work remaining is:

1. Migrate old posts over. That should be pretty easy.

2. Decide how much I care about URI compatibility and SEO "link
   juice". Specifically, is it OK if
   `blog.greghendershott.com/2013/03/some-title.html` changes to
   `www.greghendershott.com/2013-03-08-some-title.html`? Or do I want
   them exacto same? Obviously the latter will be harder and require
   something to do 301 redirects due to the subdomain change. Maybe
   I'll decide I don't care enough, given how new this blog is and the
   "link equity" is probably not so great. We'll see.
