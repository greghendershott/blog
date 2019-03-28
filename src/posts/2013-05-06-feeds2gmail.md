    Title: feeds2gmail
    Date: 2013-05-06T09:40:35
    Tags: Google, Racket, Atom, RSS

Recently I wrote about my [my Google Reader successor][], using
rss2email to push feeds to Gmail.

In the month since, I was still running it on my laptop. To make it
work best, it should run on a dedicated server. That way, it would
push emails even if I'm away from my laptop, and I could read them on
e.g. my phone. But before committing to setting this up on Amazon EC2,
I wanted to be sure I liked the approach.

<!-- more -->

Although I liked it, I missed having feeds in their own
folders. Sometimes, I want to catch up with a specific blog, as
opposed to a timeline mix of all of them.

Also, I was continuing to have deliverability issues, including Gmail
persisting in tagging some items as Spam.

I stumbled across a
[fork of rss2email that uses IMAP directly][]. Great idea!

I had what I thought was an even better idea: Instead of doing this in
Python, use Google Apps Script. This is a JavaScript environment that
Google provides as its answer to macro languages in Microsoft Office
products. It's surprising powerful. Among other things, you can
schedule scripts to run.  That would mean I wouldn't need a dedicated
server to `cron` the feed updates.

Also, I was excited for the opportunity to increase my experience with
JavaScript.

Unfortunately, although I got pretty far with this approach, I was
completely blocked because [Google Apps Script can't access IMAP][]. And
its own email APIs don't let you create a Gmail mail in a specific
folder (i.e. with a specific Gmail label).

I stewed over this for awhile. I looked at the rss2email source. And I
decided, well shit, if I have to code this to run on my own server, I
want to use Racket.

So I came up with [feeds2gmail][]. It's similar to the
[fork of rss2email that uses IMAP directly][], but written in
Racket.

One thing Racket doesn't have (that I could find) is a handy library
to read feeds. Sometimes I've wanted a magic FFI to Python and/or Ruby
libs. This was one of those times.

I ended up coding Atom, RSS, and RDF feed parsing myself. It works for
the feeds I've tried. I suspect this is the most fragile aspect that
will need bug fixes. Partly because I may have made mistakes, and
partly because the feeds have made mistakes.

Meanwhile, it's working for me. I have an experience in Gmail that is
_very_ close to Google Reader in the respects I care about.

[my Google Reader successor]: http://www.greghendershott.com/2013/03/my-google-reader-successor.html
[fork of rss2email that uses IMAP directly]: https://github.com/rcarmo/rss2email
[Google Apps Script can't access IMAP]: http://stackoverflow.com/questions/16149899/how-to-create-a-gmail-message-with-a-specific-label
[feeds2gmail]: https://github.com/greghendershott/feeds2gmail
