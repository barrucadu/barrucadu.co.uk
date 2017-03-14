---
title: Some Thoughts on Distributed Systems
description: A few thoughts on applying SCT to distributed systems.
---

This summer I'll be working on applying SCT techniques to distributed
systems in Haskell, so I've been thinking about how exactly to do
that. There is some literature on this which I've not read yet, so
these are just some initial thoughts.

## Programming model

If we step back and squint a bit, a distributed system is *really*
just a concurrent program where all the communication happens through
message passing. This suggests something like the following operations
for a core language:

- Send message to process `pid`.
- Block and wait for message from process `pid`.
- Block and wait for message from any process.

But these operations have an additional source of nondeterminism:
messages can be lost, or delayed and reordered. Processes can even
become disconnected entirely if the network connection fails, but
that's a little hard to incorporate into this style of testing.

So while the *programmer* only has those three operations in the core
language, as an *implementer* we can map those down to a few different
behaviours. If we assume a model where process has a queue of
unprocessed messages, then one possible instantiation would be this:

- Send message to process `pid`:
    - Append a message to the message queue of process `pid`.
    - Discard message.
- Block and wait for message from process `pid`:
    - If there are no messages from process `pid`, block and wait for
      one.
    - If there are multiple messages from process `pid`, return one at
      random (and leave the others in the queue).
- Block and wait for message from any process:
    - If there are no messages, block and wait for one.
    - If there are multiple messages, return one at random (and leave
      the others in the queue).

## Action variants

This is a little tricky (read: impossible) to implement in my dpor
library as it currently stands, as it assumes each thread has a
*unique* next action. Fixing this would require a representation
change. It would no longer be enough to talk about the thread
scheduled at each step, it would need to consider both the thread and
the lookahead, and the scheduler would need to be able to communicate
the *variant* of the action chosen to the program under test.

Once the representation allows it, it might then "just work". Although
I wouldn't be surprised if actually I needed to do something like
force all the variants to be included as to-do points when one is.

All these extra actions would cause some state-space explosion,
meaning we need some...

## Schedule bounding

Because we're modelling distributed systems, the standard bound
functions don't really make sense. Why should preemption bounding
produce meaningful results, when preemptions (and all context
switches!) are an artefact of the testing process?  We need bounds
which are actually relevant to the core language.

I have a few ideas. All of these can be interpreted globally, or
between a pair of processes. Some may work better in one situation
than the other.

- **Message-loss bounding**: *"lose no more than `n` messages."*
- **Total out-of-order bounding**: bound the sum of indices into
  message queues (with 0 being the head): *"re-order messages no more
  than `n` places overall."*
- **Max out-of-order bounding**: bounding the maximum index into
  message queues: *"re-order a single message no more than `n`
  places."*

These could be combined, in both the global and pair-process forms. So
for instance you could express something like:

- Lose no more than 20 messages overall; with
- no more than 2 between any pair of processes (in both directions);
  where
- messages between a pair of processes may not be re-ordered; but
- messages from a collection of different processes sent to a single
  one may be re-ordered relative to each other.

I need to do some reading. Although, of course, if your protocol
guarantees in-order lossless message delivery, then your distributed
system "really is" just a concurrent one!
