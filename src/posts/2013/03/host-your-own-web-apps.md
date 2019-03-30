    Title: Host your own web apps?
    Date: 2013-03-26T09:48:57
    Tags: software

In general [I'm a big fan of web apps][web apps] for many reasons:

- You don't need to migrate data when you buy a new computer or mobile
  device.

- You don't spend a bunch of time updating apps for security or
  improvements. Doing so may be easier on a mobile device than on a
  desktop. But there are days when my phone does more work updating
  apps than me actually using it.

- [Synchronization sucks][]. Using multiple native apps with local
  stores requires synchronizing state. Essentially this is N/A with a
  web app because the data is stored in one place. (Note: _Caching_
  doesn't suck; just synchronizing.)

- Developers can iterate and experiment -- "release constantly". This
  may sound like it's an advantage for devs, but to the extent it
  makes better apps and saner feature accretion, it's good for us,
  too.

- Easier to [hack][web apps], by which I mean customize for individual
  needs and preferences.

- Easier (and generally safer) to use hacks created by others (in the
  form of browser extensions).

So great. But obviously the [Google Reader shutdown][] brought me up
short. Shook my faith. Am I wrong to prefer web apps?

On reflection, there are two reasons why it's still OK to prefer web
apps.

<!-- more -->

The satisfactory answer is it's OK provided you can
[export your data][how-to-geek]. Many Google services let you do this,
including Google Reader. (Google's new Keep service doesn't,
yet. Until it does, you might want to steer clear, for example.)

I say the merely "satisfactory" answer because it's not ideal. There
is still that feeling that "someone else is in control of my
data". That leads to issues of trust, as well as longevity. And even
if you can take your data away, you need to deal with such
schlepping. We wanted to avoid that, with web apps.

Is some hybrid possible--a way to get the best of both worlds?

It's not clear to me yet. But let's start here: What if people could
run their own web apps on their own Amazon EC2 instances?  You can do
this today, using prebuilt images.

Unfortunately it's too complicated for an individual.  (Remember, we
prefer web apps because it gets us _out_ of playing IT
ourselves). Also for an individual even a small EC2 instance is too
much for one web app; it will be severely under-utilized. Which is
another way to say it won't be cost-effective.

But what if there were something more granular than entire EC2
instances? And what if it could be _managed_ for me, but _I_ clearly
_own_ it?

[Google Reader shutdown]: http://googlereader.blogspot.com/2013/03/powering-down-google-reader.html
[web apps]: http://www.greghendershott.com/2013/02/fucking-suggested-post-why-web-apps-matter.html
[Synchronization sucks]: http://tiamat.tsotech.com/synchronization-sucks
[how-to-geek]: http://www.howtogeek.com/141754/what-google-readers-shutdown-teaches-us-about-web-apps/

