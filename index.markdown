I am Michael Walker, otherwise known online as barrucadu. I am a
Computer Science Ph.D Student at the University of York. Although I'm
currently on a little break and interning at
[Pusher](https://www.pusher.com) in London this summer, writing Go.

My research relates to nondeterministic concurrency, and I'm doing my
work in Haskell. Increasingly, turning to concurrency for performance
gains is a no-brainer, but it can be hard to get right. Concurrency is
hard in a number of areas:

- Testing, due to nondeterminism inherent in the underlying primitives
  and abstractions, meaning we need new techniques.
- Formal verification, in the presence of abstractions like
  first-class functions, lazy evaluation, and "higher-order" state;
  many of which are essential in a pure functional language.
- Optimisation, as compiler optimisations typically stop as soon as
  side-effects are involved.

I recently submitted a paper on checking temporal logic properties of
concurrent Haskell programs to [RV'16](https://rv2016.imag.fr). Next I
plan to look at testing distributed systems with lossy communication
channels, such as UDP. Such channels have less overhead than lossless
ones, and so if you need rapid data transfer and can tolerate a bit of
loss, they're a good choice. They also underly lossless protocols,
such as TCP.

See my [publications](publications.html) and [CV](cv.pdf). If you're
looking for something to work on, see my
[project ideas](projects.html).

---

Email
: mike@barrucadu.co.uk

IRC
: barrucadu on Freenode

GPG
: 2540 0995 1D49 0628 1780  CC5E D50F C531 7F5A 95FD

---

## Recent Posts ([All Posts](posts.html))
