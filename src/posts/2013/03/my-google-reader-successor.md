    Title: My Google Reader successor
    Date: 2013-03-29T13:01:50
    Tags: Google, Atom, RSS

Just a brief update about what I've settled on as my replacement for
Google Reader.  I'm using Rss2Email, following the instructions in
[Turning Gmail into Google Reader](http://wcm1.web.rice.edu/turning-gmail-into-google-reader.html).

I tried Feedly, NewsBlur, and Reeder. Each of them wasn't bad, but
each felt "heavy" compared to Google Reader. So W. Caleb McDaniel's
post really clicked with me.

<!-- more -->

I did end up trimming 350 feeds down to 50. What I cut:

1. Old/dead feeds.

2. Big name feeds.

By the latter, I mean that I don't need feeds to help me keep up with
fire hose sites like Wired, TechCrunch, and so on. I can follow them
in Twitter. If I miss a specific article, no big deal. Also, I will
encounter a lot of that stuff on something like Hacker News.

Instead, I'm using feeds for the "long tail" stuff: Sites that post
infrequently, and/or that are are smaller. And sites where I may want
to read every post (or at least glance at every one).

The experience in Gmail is pretty darn close to GReader. Same
navigation keys. Can star things. Can forward in email. On the one
hand, this makes me empathize with Google wanting to kill Reader. But
on the other hand, it makes me feel, why didn't they add feed reeding
_into_ Gmail?  Why the desire to deprecate RSS and Atom feeds?
Grumble, grumble.

So overall it's actually quite similar. Differences in the experience?

One small nuisance is the lack of a `V` key to go to the original web
page. I have to mouse up and click the link.

A broader difference is that, even though it's Gmail and I'm far from
running out of space, I don't want a lot of feed stuff to accumulate
there.  It probably doesn't matter, I could periodically just nuke all
the mails labeled Feeds, but there's some "psychic weight".  But as I
explained, I'm being more selective in the feeds I follow.

A tip: On Mac OS X, I tried both methods of sending mail--connecting
to my GMail account, or, using `sendmail`. The latter worked better,
because the display name of the From email address is the name of the
feed (it looks like Caleb's screen shot).

For the future: I have `cron` running this hourly on my Mac Book. That
works just fine at home. But on the road, it won't be pushing emails
to my phone if the laptop isn't running. So I'll probably move this to
a server, such as a micro EC2 instance that I'm using for something
else. The only gotchas there? Might need to send the email using
Amazon SES since EC2 instances are usually blacklisted for email
sending. Plus, adding or removing feeds will require `ssh`-ing into
that server. Although not horrible, not super user friendly.

> **UPDATE:** I ended up replacing rss2email with [feeds2gmail, which is similar but has a few advantages](http://www.greghendershott.com/2013/05/feeds2gmail.html).

