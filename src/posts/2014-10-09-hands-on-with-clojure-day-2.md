    Title: Hands-on with Clojure day 2
    Date: 2014-10-09T10:58:46
    Tags: Clojure, Hacker School

As with [yesterday's post], important disclaimers:

[yesterday's post]: http://www.greghendershott.com/2014/10/hands-on-with-clojure.html

- I've used [Racket] heavily but not Clojure.

- Opinions expressed herein are not those of my employer, were I to
have one.

- If indignation lasts more than four hours, please seek medical
attention.

[Racket]: http://www.racket-lang.org

Day 2 with Clojure was much more fun! I didn't hit speed bumps with
tooling and workflow, so frequently. I was able to focus mostly on the
code itself, which was wonderful.

For a _slightly_ more realistic task, I decided to make a really
simple function that, given a URL, would make a `GET` request and
return the value of the `Server` response header (if any), and a list
of URLs found on the page that are for _other_ servers. The idea
being, you could crawl from some starting point and accumulate some
data about web server technology.

<!-- more -->

To do this, I needed to understand how to:

1. Make HTTP requests.

2. Parse an HTTP response entity (the HTML document) in a way I could
   walk it to look for `<a href="foo">` elements.

3. Miscellaneous other things.

# Making HTTP `GET` requests

I discovered `clj-http`. Added it to `projects.clj`, and added
`(:require [clj-http.client :as client])` to my `core.clj`. I got
errors in cider. It worked fine with `lein repl` at the command line.
I did a `cider-restart` and eventually it started it working.

Take-away: I'm starting to learn how to supplicate the system
reliably, with certain incantations. But I don't yet have a good
mental model for the statefulness of adding a lib to a project and
using it, at least not with cider.

Using `clj-http` was a joy. Returns a nice map with key/value pairs
I'd expect.

# Parsing and walking HTML

Next, how to parse and walk the HTML in the `:body`?

In Racket we typically use what are called `x-expressions` to
represent XML and (well-formed) HTML:

```racket
`(html ()
  (head ())
  (body ()
        (p ([attr "val"][attr2 "val2"])
           "Some text " amp " some more.")))
```

In other words this is an s-expression that follows a convention:
There is a list. The first element is a symbol for the element tag
name. The second element is an association-list of attributes. The
zero or more remaining elements are either plain data like strings or
numbers or symbols, or, other x-expressions.

I searched around and read about Enliven. That seemed like a heavier
tool than I needed. On StackOverflow someone mentioned `clj-tagsoup`.
That looked like exactly what I wanted -- or at least was familiar
with. To me, these look like x-expressions, but using vectors instead
of lists, and using maps instead of association lists.

```clj
[:html {}
 [:head {}]
 [:body {}
  [:p {:attr "val", :attr2 "val2"}
  "Some text & some more."]]]
```

Wonderful.

Adding that to my `project.clj` and requiring it in my `core.clj` was
again weird. From the GitHub README and from Clojars.org, I'm not
always sure when to use an organization-id prefix, and if so, what to
use. I must be confusing myself because this seems like it should be
simpler. At this stage, I try things until it works. The only catch
being, I might need to use `cider-restart` each time, otherwise I
might not realize I actually did get it correct.

Anyway, once added and required properly, `clj-tagsoup` was also a joy
to use.

# Parsing URLs

Now that things were working, I noticed that the list of URLs included
many links to the same server. There's no point in crawling those --
presumably the same web site is using the same web server. Instead I
want to filter these to be only URLs for _other_ sites, so we can go
crawl those and discover what _they_ return for `Server:`.

To do this, I want to split the URL into its components, and look at
the scheme (a.k.a. protocol like "http"), hostname, and port, but
disregard the rest. In Racket I would use `string->url`, which would
return a `url` struct having fields for each of these. How to do this
in Clojure?

I couldn't find an answer in the Clojure docs. For a minute I thought
about using a regular expression. But I've seen the regexp that
Racket's `string->url` uses. A _correct_ regexp is non-trivial. Anyway
this seems like a wheel I should not be reinventing.

I asked on #clojure IRC. Justin Smith and David Nolen quickly helped
me out, showing me I could use `(bean (java.net.URL.
"http://www.google.com"))`, which returns a map, members of which
include `:protocol`, `:host`, `:port`. Perfect.

Then I looked again at the map:

```clj
{:path "",
 :protocol "http",
 :ref nil,
 :content #<HttpInputStream sun.net.www.protocol.http.HttpURLConnection$HttpInputStream@56e01e8a>,
 :authority "www.google.com",
 :file "",
 :port -1,
 :host "www.google.com",
 :class java.net.URL,
 :query nil,
 :defaultPort 80,
 :userInfo nil}
```

Hmm, what's up with that part, `:content #<HttpInputStream
sun.net.www.protocol.http.HttpURLConnection$HttpInputStream@56e01e8a>`.
That sure looks like an open connection. I don't want to open a
connection for all of these -- just parse/split the URL string into
its components.

It seems that `bean` calls all of the accessor members, and [Java.Net.URL.getContent()](http://docs.oracle.com/javase/7/docs/api/java/net/URL.html#getContent%28%29) has a side-effect:

```java
openConnection().getContent()
```

So that's not good. Back on IRC, just as I was typing, "I wonder if I
can call the methods directly instead of using `bean`", Justin Smith
said the same. Turns out he'd already posted a benchmark of `bean` vs.
directly calling select members, which is just what I needed.

Of course on my first attempt I managed to forget the trailing `.` in
`(java.net.URL. url)` -- but then figured that out.

Take-aways:

- I needed to learn about Java interop earlier than I expected. The
interop _per se_ seems simple enough, and I'll re-read that part of _Joy
of Clojure_ today.

- I'm a bit worried I don't yet know the Java library ecosystem. How
will I know what's available? But I guess library discoverability is a
challenge for every language and language-learner. Maybe for Clojure
the steps will be:

    1. Search Clojure docs.
    2. Search Clojars.
    3. Search Java docs.
    4. Ask on #clojure. Preferably as step 4 not 0. :)
    
- Justin Smith suggested the
  [javadoc-search-pane](https://chrome.google.com/webstore/detail/javadoc-search-frame/mfgkihmpcjjnijmkchojlbccbmhcdfcd?hl=en)
  browser add-on for Chrome and Firefox as a great way to explore Java
  docs.

- Justin Smith is incredibly helpful! Thanks!

# Forward declarations

One thing I find slightly annoying in Clojure is the need to use
`declare`, which isn't necessary in Racket. If you like your source
code to be "bottom up", building up ingredients to a dramatic finale
at the end of the source file, it's not an issue. However, if you like
to be "top-down", starting with the main public product, and working
down into the supporting cast, it can be awkward.

# Code

```clj
(ns web-client.core
  (:require [clj-http.client :as client]
            [clojure.test :refer :all])
  (:use pl.danieljanus.tagsoup))

(defn- server
  [response]
  (get (:headers response) "Server"))

;; Grumble: Needing to use `declare` is annoying compared to Racket.
(declare body-elements
         hrefs)

(defn- urls
  [response pred]
  (let [entity (:body response)
        tree (parse-string entity)
        bodies (body-elements tree)]
    (hrefs bodies pred)))
    
(defn- body-elements
  [tree]
  (some (fn [node]
          (if (= :body (tag node))
            (children node)))
        (children tree)))

(defn- hrefs
  "Given a collection of body elements, and a predicate, return all of
  the link URLs on the page satisfying the predicate. For example the
  predicate might be if the URLs are for another hostname, i.e. a link
  to some other web server. "
  [xs pred]
  (filter pred
          (flatten
           (map (fn [x]
                  (if (coll? x)
                    (if (= :a (tag x))
                      (:href (attributes x))
                      (hrefs (children x) pred))))
                xs))))

;; TODO: Move this test elsewhere.
(is (= (find-hrefs [[:p {} "par"]
                    [:a {:href "http://hi.com"} "hi"]
                    [:p {}
                     "par"
                     [:a {:href "http://there.com"} "there"]
                     "par"]])
       ["http://hi.com"
        "http://there.com"]))

;; Grumble: Needing to use `declare` is annoying compared to Racket.
(declare valid-url? scheme+host+port)

(defn- server-equal?
  "Predicate for two URLs having 'the same server', as determined by
  having the same scheme, host, and port.

  If either URL is malformed (according to java.net.URL) then this
  returns t. That's because the intended use of this is by
  `get-server-and-links'. Of course that means the name
  `server-equal?` isn't quite right, and probably all this should be
  refactored. TO-DO."
  [a b]
  (or (not (valid-url? a))
      (not (valid-url? b))
      (= (scheme+host+port a)
         (scheme+host+port b))))

(defn- valid-url?
  [url]
  ;; Note: java.net.URL can throw an exception if the string is not a
  ;; well-formed URL. In that case, return some default map.
  (try 
    (java.net.URL. url)
    (catch java.net.MalformedURLException e nil)))

(is (valid-url? "http://www.google.com"))
(is (not (valid-url? "#anchor")))

(defn- scheme+host+port
  "Given a URL, return a map of its scheme, host, and port."
  [url]
  (let [loc (java.net.URL. url)]
    {:scheme (.getProtocol loc)
     :host (.getHost loc)
     :port (.getPort loc)}))

;; TODO: Move this test elsewhere
(is (= (scheme+host+port "http://www.google.com")
       (scheme+host+port "http://www.google.com/path/to/foo?q=0;p=1")))
(is (not (= (scheme+host+port "http://www.google.com")
            (scheme+host+port "http://www.giggle.com"))))
(is (not (= (scheme+host+port "http://www.google.com")
            (scheme+host+port "https://www.goggle.com"))))
(is (not (= (scheme+host+port "http://www.google.com:80")
            (scheme+host+port "http://www.goggle.com:8080"))))

;; TODO: Move this test elsewhere
(is (= (scheme+host+port "http://www.google.com")
       {:scheme "http", :host "www.google.com", :port -1}))
(is (= (scheme+host+port "http://www.google.com/path/to/foo?q=0;p=1")
       {:scheme "http", :host "www.google.com", :port -1}))

(defn get-server-and-links
  "Given a URL, visit it and return a map with the value of the Server
  response header (if any), and a collection of URLs for other hosts
  to which it links (if any).

  In other words, the intended use is that you'd crawl and accumulate
  data about web server technology for some subset of teh interwebs."
  [url]
  (let [response (client/get url)]
    {:server (server response)
     :urls (urls response (complement (partial server-equal? url)))}))

(comment
  (println (map get-server-and-links '("http://www.google.com"
                                       "http://clojure.org"
                                       "http://www.racket-lang.org"))))
```

# Next Steps

UPDATE: Oops, I posted without this part.

What next? Given that:

- I'm a roll with HTTP stuff in Clojure.
- I've done wrappers for web services like [AWS] and [GAPI] in Racket.
- [Hacker News] has a new, simple [REST API].

[AWS]: https://github.com/greghendershott/aws
[GAPI]: https://github.com/greghendershott/gapi
[Hacker News]: https://news.ycombinator.com/news
[REST API]: https://github.com/HackerNews/API

I think I might try writing wrappers libraries for Hacker News in both
Clojure and Racket. As a plus, I can name them `clacker-news` and
`racker-news`. I mean, maybe that's a plus.

Thursdays are alumni days at [Hacker School], and if I understand
correctly there are lightning talks (maybe this afternoon?). So maybe
I'll have less time to work on stuff today. But I can carry it over to
tomorrow, as well.

[Hacker School]: https://www.hackerschool.com
