    Title: Applicable symbols
    Date: 2014-10-27T12:29:15
    Tags: Clojure, Hacker School

Here are my notes about being puzzled about some Clojure code and
diving into the implementation to figure it out. Although I figured it
out the hard way, the exploration turned out to be interesting for me.

<!-- more -->

As I was reading
[Geoff Shannon's blog post](http://www.zephyrizing.net/blog/2014/10/14/adventures-with-clojure-macros/),
I was thinking, "Oh, maybe this will turn out to be a mixup between
compile time and run time". (At least, in my experience writing macros,
it can be challenging to keep that straight.)

Instead, what confused me was line 103 in the last example (here I've
changed the symbol from `'+` to `'x`):

```clj
(apply 'x [1 2]) ;=> 2
```

Huh? I don't understand. Why does this return `2`, instead of raising
an error?

First, let's factor out `apply` and confirm the same result for a
direct application:

```clj
('x 1 2)   ;=> 2
```

Same result. Same mystery.

Let's explore some examples:

```clj
('x)       ;=> ArityException Wrong number of args (0) passed to: Symbol  clojure.lang.AFn.throwArity (AFn.java:429)
('x 1)     ;=> nil  ???
('x 1 2)   ;=> 2    ???
('x 1 2 3) ;=> ArityException Wrong number of args (3) passed to: Symbol  clojure.lang.AFn.throwArity (AFn.java:429)

(type 'x) ;=> clojure.lang.Symbol
```

There's another mystery -- the arity 1 case of `('x 1)`.

To summarize so far: It appears that it is not an error to use a
symbol in the function position of an application if the arity is 1 or
2.

I'm aware that Clojure collections are applicable. And I'm aware that
Clojure tends to `nil` pun. So the arity 1 case might be less
surprising... except that neither `'x` nor `1` is a collection, so
this seems like it should raise an exception, not evaluate to `nil`.
And I don't understand the arity 2 case.[^arity-2]

[^arity-2]: Spoiler: By the end of this, I remember there's an
optional not-found argument.

---

What to do?  I could ask for help. But let's take this as an opportunity to go spelunking in some Clojure implementation code I've never seen before.

From looking at
[the source for clojure.lang.AFn](https://github.com/clojure/clojure/blob/201a0dd9701e1a0ee3998431241388eb4a854ebf/src/jvm/clojure/lang/AFn.java),
it seems that all `invoke` members call `throwArity`. It's been some
years since I did C++, and I'm not up-to-speed on Java. But if I
understand correctly, `AFn` is a sort of default class that always
errors. Applicable things (functions, keywords, collections) probably
derive from `AFn` and override `invoke` members to do something other
than error. Somehow such a class is created for the symbol `'x`, and
I'm guessing that it overrides the arity 1 and 2 variants of `invoke`.

That seems plausible. So, how to find this in the Clojure source?
Instead of searching the code on GitHub, let's `git clone` the source
locally and use Emacs `rgrep` to search for `invoke`. I'm sure there
are Java source navigation tools, but probably not worth
choosing/installing/learning for something this straightforward. Yes
`rgrep` returns 1200+ matches, but flipping through them quickly there
are some obvious patterns -- huge swaths that seem OK to ignore, and a
few names that pop out.

For example in `APersistentMap.java` I see arity 1 and 2 definitions
of `invoke`:

```java
public Object invoke(Object arg1) {
	return valAt(arg1);
}

public Object invoke(Object arg1, Object notFound) {
	return valAt(arg1, notFound);
}
```

That sure seems how `({:key val} :key)` and `({:key val} :key
default)` would work. `valAt` is a core member function that does the
lookup of a key and value in the map.

OK. Next let's try to find a class that might correspond to a symbol
like `'x` and examine its `invoke` member(s).

And bingo, here's a file called `Symbol.java`, also defining arity 1
and 2 variants of `invoke`:

```java
public Object invoke(Object obj) {
	return RT.get(obj, this);
}

public Object invoke(Object obj, Object notFound) {
	return RT.get(obj, this, notFound);
}
```

So this seems to explain why arities 1 and 2 are special. Next
question: What is `RT.get`? In C++ this would mean a static `get`
member of an `RT` class. That seems to be the case here, looking in
`RT.java`. `get` comes in both arity 1 and 2 flavors (not counting the
first argument; this is a static member so the implicit `this`
argument is explicit). First here's arity 1:

```java
static public Object get(Object coll, Object key){
	if(coll instanceof ILookup)
		return ((ILookup) coll).valAt(key);
	return getFrom(coll, key);
}

```

Which of the two paths here would `'x` take? I'm going to make a guess
that a symbol probably isn't an instance of `ILookup`, and what's
happening is the call to `getFrom`. Which is defined in `RT.java` as:

```java
static Object getFrom(Object coll, Object key){
	if(coll == null)
		return null;
	else if(coll instanceof Map) {
		Map m = (Map) coll;
		return m.get(key);
	}
	else if(coll instanceof IPersistentSet) {
		IPersistentSet set = (IPersistentSet) coll;
		return set.get(key);
	}
	else if(key instanceof Number && (coll instanceof String || coll.getClass().isArray())) {
		int n = ((Number) key).intValue();
		if(n >= 0 && n < count(coll))
			return nth(coll, n);
		return null;
	}

	return null;
}
```

My guess is that all of the conditional branches fail for a symbol
like `'x`, and `getFrom` returns Java `null`. Which becomes Clojure
`nil`. First mystery solved.

How about arity 2? This is the case where a "not-found" value is
supplied:

```java
static public Object get(Object coll, Object key, Object notFound){
	if(coll instanceof ILookup)
		return ((ILookup) coll).valAt(key, notFound);
	return getFrom(coll, key, notFound);
}

static Object getFrom(Object coll, Object key, Object notFound){
	if(coll == null)
		return notFound;
	else if(coll instanceof Map) {
		Map m = (Map) coll;
		if(m.containsKey(key))
			return m.get(key);
		return notFound;
	}
	else if(coll instanceof IPersistentSet) {
		IPersistentSet set = (IPersistentSet) coll;
		if(set.contains(key))
			return set.get(key);
		return notFound;
	}
	else if(key instanceof Number && (coll instanceof String || coll.getClass().isArray())) {
		int n = ((Number) key).intValue();
		return n >= 0 && n < count(coll) ? nth(coll, n) : notFound;
	}
	return notFound;

}
```

Aha. `('x 1 2)` is being treated as, "Look up `1` in `'x` and if not
found return `2`".

# Editorializing

It makes sense to support a symbol in the function position of an
application to enable doing this:

```clj
('key {'key 42})    ;=> 42
('key {'key 42} 42) ;=> 42
('key {}        42) ;=> 42
```

Of course. That's awesome.

But when the second element is _not_ a collection? It should error.
Looking up a key in an integer doesn't make any sense. It is the
result of a mistake. Clojure should tell you it's an error. Returning
something here is as bad as silent coercions in Javascript.

# Postscript

For what it's worth, here's how this works in
[`rackjure`](https://github.com/greghendershott/rackjure):

```racket
#lang rackjure

('key {'key 42})           ;=> 42
('key {'key 42} #:else 42) ;=> 42
('key {}        #:else 42) ;=> 42

('key 1)
; applicable-dict: No dict? supplied
; in: ('key 1)
; Context:
;  /tmp/foo.rkt:1:1 [running body]
```

One difference is that arity 3 means `assoc`. If you want to supply an
explicit default, you use an `#:else` keyword argument.

The main difference is that a nonsense application results in an
error.
