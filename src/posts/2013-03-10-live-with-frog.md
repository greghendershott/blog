    Title: Live with Frog
    Date: 2013-03-10T19:14:44
    Tags: blog, Frog, Racket, software

OK, I have my site generated using Frog and pushed it to GitHub Pages
for <http://www.greghendershott.com>.

I want to tweak the CSS. It's vanilla Bootstrap. Most importantly, I
wish the headings were a few points smaller and the body text a few
points bigger.

The other remaining item is to do the 301 redirect from Blogger to
here. It turns out that this should be ridiculously easy to do with
Amazon S3. You can make
[a bucket that does nothing but perform 301 redirects][1]. I've
created such a bucket already, and the redirects work fine. I simply
need to update my DNS, for which I'm using Amazon Route 53.

[1]: http://docs.aws.amazon.com/AmazonS3/latest/dev/website-hosting-custom-domain-walkthrough.html
