Well, one month tomorrow, but near enough.

School has ended for half term week—a whole 5 week days off, and what shall I do with this vast amount of free time on my hands? I have a couple of plans. Firstly, **Erasmus**. I'd like to hit version 0.1 and make a dent in 0.2 this week, as I have free time I can't really hit a snag and put off solving it indefinitely due to “schoolwork”.

Remaining 0.1 features:

 * Working memory manager
 * IDE driver
 * ATA driver

The IDE and ATA drivers have sort of merged, which is ok, for now; I can separate them when I start on ATAPI. The memory manager is there, but doesn't work: I'm going to write one from scratch rather than just borrowing/editing code, as then I'll definitely know what's going on.

After Erasmus, **fractalgen**. I want to figure out how to create arbitrarily large images with gd; if I try to make one too big it complains about the size exceeding INT_MAX, or something like that. I refuse to believe gd has such a limitation that has no work-arounds when it comes to large images. I also want to start on my fviewer program, a GUI to display / zoom fractals. Smooth colour shading would be nice, but it's not really a high-priority feature currently.

Of course, there are always **books**. I've still got a heap of books from Christmas to read, and with 9 days before school again, I should be able to get through at least one, perhaps two. I make it sound like I try to read books as quickly as possible, which isn't entirely true: I just dislike having unread books lying around, when I re-read books, I do take my time and generally notice things I didn't the first time through.

Leading on to other forms of entertainment: **anime**. I have borrowed the first two seasons of *Claymore* which, hopefully, I'll enjoy. *Claymore* is about a world in which humans coexist with *yoma*, demons, where there exists a group of half-human half-*yoma* warriors, the Claymores, who protect the humans (for a fee).

Finally, after mpdspl was rewritten to be much better by [sdelafond](http://github.com/sdelafond), I have been spurred into action to improve **mpddp**. A complete rewrite; a second version. It will be object-orientated and have much better code organisation (the current program is a mess: why I change it so very rarely), and I have a vague notion of wanting a “more powerful” config syntax. Currently each line of a config file acts as a boolean OR: including everything which matches this line, OR that line, OR another… I want to include more boolean operations, AND at the very least, perhaps NOT (that's currently worked-around by the “never” keyword) or XOR, too.

Of my plans, mpddp 2 is the one I'm most excited about implementing. I can barely remember how the current code works, so I'll most likely read through it to figure out which functions do what, and then just write version 2 from scratch.
