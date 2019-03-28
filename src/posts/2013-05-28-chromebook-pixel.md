    Title: Chromebook Pixel
    Date: 2013-05-28T10:07:10
    Tags: Google, Linux, Racket

I'm writing this blog post on a [Chromebook Pixel][]. In Emacs. On
Ubuntu, as a [chroot][], thanks to [crouton][].

Why do I have a Chromebook Pixel? Google gave one to every Google I/O
2013 attendee.[^free]

Although I was happy to get such a cool new gadget, I honestly wasn't
sure what I'd _do_ with the thing.  I really like my MacBook Pro
Retina and wasn't looking for an alternative. Also, although
[I love web apps][] and 45% of my day is in the web browser, another
45% is in Emacs using [Racket][] --- what about that?

<!-- more -->

Then I remembered you can run Linux on a Chromebook. Well, it's
_already_ running Linux; that's what ChromeOS is built on. For "real"
Linux, you have a choice:

1. Dual boot ChromeOS and Linux.

2. Use chroot to run some other Linux userland on top of the ChromeOS' Linux kernel.

The advantage of the latter is that you can still run ChromeOS, and
hit hotkeys to flip between it and the "real" Linux desktop.

> **NOTE**: In the rest of this blog post, keep in mind that my Linux experience has been limited to command-line environments like Amazon Linux on EC2.  My _entire_ GUI desktop Linux experience consists of using Ubuntu on the Chromebook Pixel for the last 2 days. So if I write something stupid, keep that in mind and feel free to chime in with a nice comment below. ;)

## Installing

1. Put the Chromebook into developer mode.[^dev] ~15 minutes.

2. Download and install [crouton][]. ~30 minutes.

3. [Configure Ubuntu][] to deal with the Chromebook Pixel's insanely high DPI.

## Emacs

As I write this, the newest Emacs available from `apt-get` is
23.3. On OS X, I'd grown used to 24.2, including its package
manager. Otherwise, a simple `git clone` of my `.emacs.d` tree got
everything working quickly.

I realize it's possible to build a newer Emacs. Just haven't gotten to
that.

## Racket

The `Linux x86_64 (Debian squeeze)` build of [Racket][] works
great. The only issues I encountered were the following:

### DrRacket

Just crank the font size up `CTRL +` a few notches. Bad news: Some UI
elements like toolbar buttons will remain tiny. Good news: The main
text in the editor is rendered cripsly (unlike on the MacBook Pro
Retina display where it's some 4x4 sub-pixel affair).

### send-url (and DrRacket help)

By default, `send-url`[racket] from `net/sendurl` doesn't know how to
start a browser. If it used `reasonable-browser` or `xdg-open` I think
it would work. It looks like it's possible to deal with this by
changing the `external-browser`[racket] parameter. For example, here's
a quick hack I put in my local build of [Frog][] to get its preview
feature working:

```racket
  ;; This is a bit of a hack: If `preview` fails on Linux, try again
  ;; using the "sensible-browser" command.
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (match (system-type 'os)
                       ['unix
                        (parameterize ([external-browser '("sensible-browser " . "")])
                          (preview))]
                       [_ (eprintf "~a" (exn-message exn))]))])
    (preview))
```

This is _not_ necessarily the right way to handle this; I might not
even commit this code.

Also, this doesn't fix DrRacket knowing how to start a browser to show
documentation.

## CIRC

When I searched the Chome store for "IRC", strangely it did _not_ show
me [CIRC][]. Fortunately I found it from regular Google search. Very
nifty IRC client. Not as featureful as something like Colloqy on OS X,
but neat. Of course if you want to run IRC on the Ubuntu chroot
instead of the main ChromeOS, there are the usual choices.

## Prognosis?

I spent a couple decades using DOS and then Windows. Only in the last
few years have I seriously used OS X full-time, and used command-line
Linux on EC2 instances. I've wanted to try a GUI Linux. Also, as I've
grown grumpy lately about Apple and Google wanting to put us into AOL
era walled gardens, I've thought more about using Linux as my main OS.

But I love laptops. And I hate screwing around with drivers. And I
_really_ hate laptops where things like wi-fi, power management, and
the touch pad don't really work correctly. So I've never wanted to go
down that road of finding some specific ThinkPad model from 2007,
spending a week futzing with drivers, and spending years with the
thing never quite working right. At least that's the impression I get
everytime I research this. I won't get a hobbyist's enjoyment from
tinkering with the basic damn hardware functionality of my main
working machine. I need it to work, so I can focus on the kind of
tinkering I _do_ like.

So as far as I could tell, I'd probably never go down that road. And
yet, hello, Chromebook Pixel. So far, it just-works the way I would
want a Linux laptop to just-work. With a screen that's even more
beautiful than the MacBook Pro Retina (and, although it's more of a
novelty, for me, it's a flipping _touch_ screen). This thing is sweet.

The irony is that this laptop is from Google, who I mentioned above is
one of the reasons why I was interested in moving to Linux. Plus, it's
Google, which means in 2 years will the whole thing get kicked to the
curb like Reader and the latest round of open standards? Even if
ChromeOS survives, this weird and wondeful Pixel-class Chromebook
might not. It feels like something for early-adopter developers, a
happy accident that will get cleaned up someday in an orgy of
strategory changing and focusing. I hope I'm wrong. I hope this hot
mess survives.



[Chromebook Pixel]: http://www.google.com/chromebook/pixel
[chroot]: http://en.wikipedia.org/wiki/Chroot
[crouton]: https://github.com/dnschneid/crouton
[Configure Ubuntu]: https://github.com/dnschneid/crouton/wiki/Chromebook-Pixel
[Racket]: http://racket-lang.org/
[I love web apps]: http://www.greghendershott.com/2013/02/fucking-suggested-post-why-web-apps-matter.html
[Frog]: https://github.com/greghendershott/frog
[CIRC]: https://chrome.google.com/webstore/detail/circ/bebigdkelppomhhjaaianniiifjbgocn?hl=en-US


[^free]: Free? Well, the Google I/O fee was $999. Plus travel and hotel. So it wasn't free for me in that sense. However the Pixel is $1299, Google's COGS is probably ~$1000, and running Google I/O costs something. So it was at least free-ish, and certainly a very nice surprise from Google.

[^dev]: Otherwise known as "can't watch the Bluths" mode. Literally the day after I flipped the Chromebook into dev mode, _Arrested Development_ premiered on Netflix. But Netflix disables video viewing for dev mode. Amazon Instant Video, interestingly, still works.
