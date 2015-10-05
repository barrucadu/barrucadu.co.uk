---
title: Paper Summary: Secure Communications Over Insecure Channels
description: Review of (Merkle 1978), one of the first public-key cryptosystems.
---

We're trialling a "classic paper reading group" at [HackSoc][] this
term, where we meet once a fortnight to discuss a paper which was
particularly influential, approachable, or just really
well-written. To help prepare, I've decided to write little summaries.

[HackSoc]: http://hacksoc.org/

## Secure Communications Over Insecure Channels ([Merkle 1978][])

> According to traditional conceptions of cryptographic security, it
> is necessary to transmit a key, by secret means, before encrypted
> messages can be sent securely. This paper shows that it is possible
> to select a key over open communications channels in such a fashion
> that communications security can be maintained. A method is
> described which forces any enemy to expend an amount of work which
> increases as the square of the work required of the two communicants
> to select the key. The method provides a logically new kind of
> protection against the passive eavesdropper. It suggests that
> further research on this topic will be highly rewarding, both in a
> theoretical and a practical sense.

This is one of the first papers on public-key[^pubkey] cryptography,
based on solving puzzles: small ciphertexts designed to be broken. The
protocol was originally devised in 1974, and a revised version of the
paper published in 1978. The paper starts by talking about the
problems with traditional crypto methods, which can be summarised as
two points:

1. Traditional crypto requires a secret key, known only to the
   legitimate participants.
2. Traditional crypto assumes the existence of a totally secure
   channel in order to distribute this key.

The solution is to *not* have a secure channel! The contribution of
this paper is the idea that even when an attacker has perfect
information of all the communications, a secure key can still be
decided upon by the participants without an attacker being able to
easily get it. More precisely, that an attacker would have to put in
significantly more work than the participants to determine the key.

In this algorithm, the attacker needs to put in O(N^2) work, whereas
each participant only needs O(N) work.

If we call the two participants Alice and Bob, the key decision
process goes like this:

1. Alice and Bob agree on some number N.
2. Alice generates N puzzles, where the work required to break a
   puzzle is O(N). More specifically:
    - A puzzle is an encrypted string consisting of a random ID number,
      a random key, and some constant string.
    - Encryption is done by using some strong algorithm and
      restricting the size of the key space to some linear function of
      N.
    - Each puzzle is encrypted with a different random key from this
      key space (note that this is *not* the same as the key included
      in the puzzle cleartext).
3. Alice transmits all the puzzles to Bob.
4. Bob picks one puzzle at random, and solves it. Specifically:
    - Bob brute-forces the key of the puzzle (this is the only possible
      method, as a strong encryption function was chosen). Bob can
      check that a puzzle was correctly decrypted by checking for the
      agreed-upon constant string.
5. Bob transmits the ID number of the chosen puzzle to Alice.
6. Alice and Bob now use the key from that puzzle for all further
   communications.

Let's introduce an attacker Eve, and summarise what they all know
after this exchange:

- **Alice** knows the N puzzles, the cleartext of all puzzles, Bob's
  chosen ID number, and the corresponding key.
- **Bob** knows the N puzzles, the cleartext of one puzzle, the ID
  number, and the corresponding key.
- **Eve** knows the N puzzles and the ID number.

The only way for Eve to get the corresponding key is to solve puzzles
at random until she finds one with a matching ID number. This will
require solving N/2 puzzles on average, which corresponds to O(N^2)
time, as each puzzle takes O(N) to solve.

[Merkle 1978]: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.364.5157

[^pubkey]: The meaning of "public-key" has shifted a bit from when
this paper was written. It looks like Merkle used it to mean that the
key negotiation happens over a public channel, but modern usage
specifically refers to asymmetric cryptosystems where keys are
distributed (publicly) in advance.

### Legacy

> There is no reason to assume an exponential method is
> impossible. [...] To attain realistic levels of security using the
> O(N^2) method would require a large value for N, which would be
> costly. An exponential method would eliminate this cost, and so be
> more attractive.

Such an exponential-time algorithm is [Diffie--Hellman][][^D-H] key
exchange, which is now the basis of a lot of modern public-key
crypto. Hellman himself suggests that the algorithm should be instead
called "Diffie--Hellman--Merkle key exchange", as Merkle first
developed the idea that you can agree on a secret key over a public
channel:

> The system [...] has since become known as Diffie--Hellman key
> exchange. While that system was first described in a paper by Diffie
> and me, it is a public key distribution system, a concept developed
> by Merkle, and hence should be called 'Diffie--Hellman--Merkle key
> exchange' if names are to be associated with it. I hope this small
> pulpit might help in that endeavor to recognize Merkle's equal
> contribution to the invention of public key cryptography[^D-H-M].

Merkle goes further than just proposing a key exchange algorithm, he
anticipates the development of publicly-known keys and keyservers! He
discusses this in the context of an organisation wishing to have
private communication in the face of an enemy, based on codebooks:

> First, each unit or command that wished to be in the code book would
> generate its own first transmission
> [the constant string and the N puzzles]. These would all be sent to
> a central site, where the names and first transmissions of all
> involved communicants would be entered into the code book. The
> codebook would then be distributed. In essence, we are simply
> specifying the nature of the communication channel between X and
> Y. It is not a direct communication channel, but is somewhat
> roundabout.  X publishes his first transmission in the codebook,
> along with his name. The return transmission from Y to X can now
> take place over normal communication channels.  Y is assured that he
> is talking to X, because Y looked up X's first transmission in the
> codebook. At this point X and Y have established a common key, but X
> does not know that he is talking to Y. Anyone could have sent the
> return transmission, claiming they were Y.  To avoid this, X and Y
> repeat the process of selecting a key, but X now looks up Y in the
> codebook, and sends a return transmisison to Y, based on Y's first
> transmission.  The return transmission will be meaningful only to Y,
> because the return transmission is based on Y's first
> transmission. X knows Y's first transmission came from Y, because it
> is entered in the codebook. If X and Y now use both keys, then they
> are assured they are talking to each other, and no one else. To
> summarize: using only a codebook, which is assumed to be correct,
> but which is not assumed to be secret, X and Y have established an
> authenticated, secure communications channel. They have done so
> quickly and easily. The key need be used for only a short period of
> time (a single conversation), and can then be changed with equal
> ease.

A more familiar discussion then follows proposing effectively the same
protocol, but in the context of computer systems. The compiler of the
codebook is the network administrator, and the codebook is the listing
of users.

It would be no exaggeration to say that, without this contribution,
public-key cryptography would have been much slower to develop, and
the state of secure communication would not be as happy as it is
today. Furthermore, like many papers introducing an entirely new
field, this one is *simple*, it's *easy to read*, and it doesn't
require a lot of background knowledge. The algorithm described can be
implemented in a few dozen lines of code. This is a strength not only
of Merkle's presentation of this material in particular, but of all
foundational papers.

And that's why we read classic CS papers.

[^D-H]: New directions in cryptography ([Diffie & Hellman 1976][])

[^D-H-M]: An overview of public key cryptography ([Hellman 2002][])

[Diffie--Hellman]: https://en.wikipedia.org/wiki/Diffie%E2%80%93Hellman_key_exchange
[Diffie & Hellman 1976]: https://dl.acm.org/citation.cfm?id=2269104
[Hellman 2002]: http://ieeexplore.ieee.org/xpl/articleDetails.jsp?arnumber=1006971

## Thoughts

Here are some discussion points if you want to talk about this paper
with others:

- The paper mentions an attacker discovering a secret key being
  transmitted over a secure channel (as in traditional crypto) by
  "practical cryptanalysis", a euphemism for physically intercepting
  the message. Public-key crypto solves this to some extent, but is
  the issue of "practical cryptanalysis" totally solved?

- This algorithm requires an attacker to put in O(N^2) work to
  determine the key, whereas the communicants only need O(N)
  work. Quadratic time isn't generally regarded as being very good for
  crypto nowadays. Why?

- As all the communication is public, an attacker could just record
  everything said and gain access to all communications past, present,
  and future when they eventually crack the key. Why isn't this a huge
  flaw with public-key cryptography?
