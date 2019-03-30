    Title: Feed stats in Frog (without FeedBurner)
    Date: 2013-03-19T09:11:43
    Tags: Frog, Google, Atom, RSS

In light of Google [shutting down Google Reader][] and
[removing feed-following UI in Chrome][], it probably can't be long
until they shut down FeedBurner, too.

Although I'm using [Google Analytics][] for this blog, I'm not using
FeedBurner. But imagining what feed readership stats I might want, I
came up with a short list, and thought about how to get them without
FeedBurner.

<!-- more -->

1. "Subscribers": Tell me how many people have chosen to subscribe to
   the Atom or RSS feed. How often they bother to read it is another
   matter.

2. "Readers on web site": Tell me how many people are clicking through
   from the feed to read the full post on the original web site.

3. "Readers, feed-only": Tell me how many people opened the feed item,
   possibly read it, but did _not_ click through to the orignal web
   site.

So the excercise is, how to do this _without_ FeedBurner.

I [just added][7c0ed36] tentative stuff to [Frog][] for items 2 and 3.


## Readers clicking through to this site

Some people will start to read an item in the feed, then click through
to the main web site to finish reading it. (People are more likely to
do this if you've set your blog to show only above-the-jump summaries,
with "continue reading" links.)

Since this is page views on our web site, this can be handled by
Google Analytics (or similar, if Google shuts _that_ down). The only
trick is to distinguish viewers who got here from the feed, as opposed
to from somewhere else.

Well, that's what the GA `utm_xxx` [query parameters][] are for. In the
feed, we'll decorate the URIs that lead back to the original blog
post.

    http://example.com/path/to/thing.html

becomes

    http://example.com/path/to/thing.html?
                          utm_source=feed&
                          utm_medium=feed

This should let us view stats for `path/to/thing.html` in Google
Analytics, that came via the feed.

In Frog I'm also adding a `utm_campaign` parameter, and distinguishing
the RSS and Atom feeds (just because I'm curious).


## Readers staying in their feed reader

Some people will solely read items in your feed, and not click
through to your web site. (This is especially likely if you're showing
full posts in your feed.)  How to count reads?

The answer here seems to be the tried-and-true "image bug".

Each feed item's contents will get the following image element added
automatically:

```html
<img height="1"
     width="1"
     src="http://example.com/img/1x1.gif?
                         utm_source=feed&
                         utm_medium=feed&
                         utm_campaign=<URI of blog post>"
```

We'll share the same 1x1.gif image bug across all posts. So we'll
distinguish each post using the `utm_campaign` query-parameter. The
"campaign" name will be the URI of the blog post.

## Subscriber counts

I've glossed over this so far. It's the least-interesting metric to
me--it seems akin to caring about followers count, as opposed to real
interactions, on a social network.

In any case, presumably the way to handle this is as a Goal in Google
Analytics: People who click the feed link have reached the goal.

If that's not exactly like what FeedBurner does, maybe that's OK with
me. If you have some other idea, let me know.

## Summary

That's what I've sketched out so far. It's taking a little time to
test it out. If you see any problems or sheer stupidity, please let me
know in the comments.


[shutting down Google Reader]: http://googlereader.blogspot.com/2013/03/powering-down-google-reader.html
[removing feed-following UI in Chrome]: http://www.webmonkey.com/2013/03/its-not-just-reader-google-kills-chrome-rss-extension-too/
[Google Analytics]: http://www.google.com/analytics/
[7c0ed36]: https://github.com/greghendershott/frog/commit/7c0ed36acf8061cc0ac7332aaa8747b184fcdd0f
[Frog]: https://github.com/greghendershott/frog/
[query parameters]: http://support.google.com/analytics/bin/answer.py?hl=en&answer=1033867
