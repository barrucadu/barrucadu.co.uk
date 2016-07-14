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

I write sometimes. See my [blog](/posts.html).

---

<div class="vspace"></div>

<h2 class="big">Employment</h2>

<div class="cventry"><div class="left"><img src="logos/pusher.png" alt="Pusher" class="small"></div><div>

### **Pusher**, Software Engineering Intern <span class="meta">(May 2016--now, London)</span>

I write Go and code which generates Go.

</div></div>

<div class="cventry"><div class="left"><img src="logos/university-of-york.png" alt="University of York" class="big"></div><div>

### **University of York**, Postgraduate Teaching Assistant (TPOP) <span class="meta">(Oct 2015--May 2016, York)</span>

Assisted lecturer with the teaching of the first year "Theory and
Practice of Programming" module by marking student homeworks and
leading small-group discussions. TPOP teaches algorithms, data
structures, and the basics of complexity theory.

### **University of York**, Postgraduate Teaching Assistant (MFCS) <span class="meta">(Oct 2014--May 2015, York)</span>

Assisted lecturer with the teaching of the first year "Mathematical
Foundations of Computer Science" module by answering student queries
during problem classes, and invigilating and marking formative
assessments. MFCS teaches simple logic, set theory, and proof; and
formal languages, grammars, and automata.

</div></div>

<div class="cventry"><div class="left"><img src="logos/corefiling.png" alt="CoreFiling"></div><div>

### **CoreFiling**, Software Engineering Intern <span class="meta">(Jul 2014--Sep 2014, Oxford)</span>

Refactored an extensively-used in-house wiki program, fixing numerous
long-standing bugs. Built a parser/renderer for a Creole-like markup
language using ANTLR in Java. Wrote a JIRA plug-in enabling the use of
this markup in issue descriptions and comments.

* <i class="fa fa-github"></i> [reviki](https://github.com/CoreFiling/reviki)
* <i class="fa fa-github"></i> [reviki-jira-plugin](https://github.com/CoreFiling/reviki-jira-plugin)

</div></div>

---

<div class="vspace"></div>

<h2 class="big">Education</h2>

<div class="cventry"><div class="left"><img src="logos/university-of-york.png" alt="University of York"></div><div>

### **University of York**

#### Computer Science (Ph.D) <span class="meta">(2014--now)</span>

Expected to submit in December 2017.

#### Computer Systems and Software Engineering (M.Eng) <span class="meta">(2010--2014)</span>

Received first-class honours for dissertation on the formal
verification of stop-the-world garbage collectors.

</div></div>

---

<div class="vspace"></div>

<h2 class="big">Open Source</h2>

<div class="cventry"><div class="left"><img src="logos/archhurd.png" alt="Arch Hurd" class="small"></div><div>

### **[Arch Hurd](http://www.archhurd.org)**, Project Leader <span class="meta">(2010--2015)</span>

Managed a small, geographically-diverse, development team porting core
Linux software to the GNU/Hurd platform. Also produced installation
media and maintained a website and online software repository.

</div></div>

<div class="cventry"><div class="left"><img src="logos/uzbl.png" alt="Uzbl" class="small"></div><div>

### **[Uzbl](http://www.uzbl.org)**, Developer <span class="meta">(2009)</span>

Was part of a small development team that implemented early-stage
functionality in an open-source web browser using C and git.

</div></div>

---

<div class="vspace"></div>

<h2 class="big">Projects</h2>

### **[Déjà Fu](https://github.com/barrucadu/dejafu)**, Concurrency testing library

- Allows the deterministic and systematic testing of concurrent
  Haskell programs.
- Supports almost all of standard Haskell concurrency.
- Has realistic execution semantics, including an implementation of
the x86 / x86_64 relaxed memory model.
- Resulted in one peer-reviewed publication.

---

<div class="vspace"></div>

<h2 class="big">Publications</h2>

### Conference papers

<ol class="links">
  <li><a href="/publications/dejafu-hs15.pdf" class="title">&ldquo;Déjà Fu: A Concurrency Testing Library for Haskell&rdquo;</a><br/>
    <span class="description">
      In <em>ACM SIGPLAN Symposium on Haskell (<a href="https://www.haskell.org/haskell-symposium/2015/">Haskell '15</a>)</em>
      [<a href="/publications/dejafu-hs15.bib">bib</a>]
      [<a href="https://dx.doi.org/10.1145/2804302.2804306"><abbr title="Digital Object Identifier">doi</abbr></a>]
    </span>
  </li>
</ol>

### Others

<ol class="links">
  <li><a href="/publications/YCS-2016-503.pdf" class="title">&ldquo;Déjà Fu: A Concurrency Testing Library for Haskell&rdquo;</a><br/>
    <span class="description">
      University of York Computer Science Department Technical Report YCS-2016-503.
      [<a href="/publications/YCS-2016-503.bib">bib</a>]
    </span>
  </li>

  <li><a href="/publications/searchparty-acmsrc15.pdf" class="title">&ldquo;Search Party: a Haskell library for speculative parallelism in generate-and-test searches&rdquo;</a><br/>
    <span class="description">
      In <em>ACM ICFP Student Research Competition (<a href="http://icfpconference.org/icfp2015/src.html">ICFP SRC '15</a>)</em>.
      Presented as a poster.
    </span>
  </li>
</ol>

---

<div class="vspace"></div>

**I also have a more select version of my [CV](cv.pdf) as a PDF.**
