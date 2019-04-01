    Title: Understanding and Using C Pointers
    Date: 2013-08-27T09:14:03
    Tags: software

![A C programmer preparing to use pointers](https://upload.wikimedia.org/wikipedia/commons/thumb/b/b7/Aviano_bomb_suit.jpg/220px-Aviano_bomb_suit.jpg)

Someone[^someone] in my Google+ circles posted about
[_Understanding and Using C Pointers_][book], a fairly recent (May,
2013) book by Richard Reese.

<!-- more -->

Here's the actual summary from oreilly.com:

> [![Understanding and Using C Pointers](https://akamaicovers.oreilly.com/images/0636920028000/cat.gif)](https://shop.oreilly.com/product/0636920028000.do)
>
> Improve your programming through a solid understanding of C pointers and memory management. With this practical book, you’ll learn how pointers provide the mechanism to dynamically manipulate memory, enhance support for data structures, and enable access to hardware. Author Richard Reese shows you how to use pointers with arrays, strings, structures, and functions, using memory models throughout the book.
>
> Difficult to master, pointers provide C with much flexibility and power—yet few resources are dedicated to this data type. This comprehensive book has the information you need, whether you’re a beginner or an experienced C or C++ programmer or developer.

On the one hand, it makes me feel old to realize how many programmers
have grown up not needing to learn this. On the other hand, it's neat
that people still want to learn it and appreciate the value of knowing
it.

Even when using a higher-level language, there will be times when it's
helpful to have a sense of what's actually going on at the lower
levels. Programming bigger systems involves various levels of
abstraction. Usually the trick is pretending that you _don't_ know how
the lower levels work---avoiding the temptation to use details about
lower levels and break the barrier of abstraction. However when things
aren't performant, it helps to use your X-ray vision and look through
the barriers.

My first (and only) two CS classes were about Pascal and
assembly. Those turned out to be perfect preparation for teaching
myself C, which in some ways is a synthesis of both.

---

One issue with C pointers is that people need to learn both new
concepts and new notation. Anyone who doesn't get a headache the first
time they see the notation for a function pointer is a weird space
alien. Combine that notation with learning the idea of "a pointer to a
function", and it's going to be difficult.

I can vaguely recall that experience learning C. Much more recently I
remember examples with, say, continuations and macros in Scheme and
Racket.

Maybe it would help to provide some training wheels that use "plain
English" instead of "weird" or "concise" notation. Or maybe not. Maybe
it's just hard and you need to slog through it.

---

Anyway, here are my _imaginary_ review quotes for _any_ book about C
pointers:

- "Casts the subject in a whole new light."

- "Thoroughly researched; references counted meticulously."

- "Helped me get a handle on pointers, albeit indirectly." (a Windows 3.1[^win] reviewer)

- "Great pointers to using pointers, albeit indirectly". (a Mac OS 9[^mac] reviewer)

- "You will learn that pointers that dangle are bad, regardless of the angle."

- "This comprehensive reference points to various arguments, some positive, some negative, some null and void."

- "Didn't like it. Tossed in garbage. Hope collected soon."﻿

Again, these are _pretend_ reviews. The _real_ reviews are very
positive and it looks like an excellent book that you should
[buy][book][^link] immediately.

[book]: https://shop.oreilly.com/product/0636920028000.do

---

[^someone]: Although I want to give them credit, they shared the post privately so I don't want to presume to name them.

[^win]: For movable allocated memory, older versions of Windows used opaque `HANDLE`s you could cash in for real pointers.

[^mac]: Whereas Mac OS used pointers to pointers, if I recall correctly.

[^link]: Not an affiliate link.
