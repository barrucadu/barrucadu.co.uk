---
title: C is not Turing-complete
description: Because of how pointers are defined in the C standard, an
  isolated C program is equivalent in computational power to a
  pushdown automata.
---

A friend told me that C isn't actually Turing-complete due to the
semantics of pointers, so I decided to dig through the (C11) spec to
find evidence for this claim. The two key bits are 6.2.6.1.4 and
6.5.9.5:

> Values stored in non-bit-field objects of any other object type
> consist of `n × CHAR_BIT` bits, where `n` is the size of an object
> of that type, in bytes. The value may be copied into an object of
> type `unsigned char [n]` (e.g., by `memcpy`); the resulting set of
> bytes is called the object representation of the value. Values
> stored in bit-fields consist of `m` bits, where `m` is the size
> specified for the bit-field. The object representation is the set of
> `m` bits the bit-field comprises in the addressable storage unit
> holding it. Two values (other than NaNs) with the same object
> representation compare equal, but values that compare equal may have
> different object representations.

The important bit is the use of the definite article in the first
sentence, "where `n` is **the** size of an object of that type", this
means that all types have a size which is known statically.

> Two pointers compare equal if and only if both are null pointers,
> both are pointers to the same object (including a pointer to an
> object and a subobject at its beginning) or function, both are
> pointers to one past the last element of the same array object, or
> one is a pointer to one past the end of one array object and the
> other is a pointer to the start of a different array object that
> happens to immediately follow the first array object in the address
> space.

Pointers to distinct objects of the same type[^heaps] compare
unequal. As pointers are fixed in size, this means that there's only a
finite number of them. You can take a pointer to any object
[^address-of], therefore there are a finite number of objects that can
exist at any one time!

However, C is slightly more interesting than a finite-state
machine. We have one more mechanism to store values: the return value
of a function! Fortunately, the C spec doesn't impose a maximum stack
depth[^recursion], and so we can in principle implement a pushdown
automata.

Just an interesting bit of information about C, because it's so common
to see statements like "because C is Turing-complete...". Of course,
on a real computer, nothing is Turing-complete, but C doesn't even
manage it in theory.

[^heaps]: Interestingly, you could have a distinct heap for every
  type, with overlapping pointer values. And this is totally fine
  according to the spec! This doesn't help you, however, because the
  number of types is finite: they're specified in the text of the
  program, which is necessarily finite.

[^address-of]: "The unary `&` operator yields the address of its
  operand.", first sentence of 6.5.3.2.3.

[^recursion]: "Recursive function calls shall be permitted, both
  directly and indirectly through any chain of other functions.",
  6.5.2.2.11, nothing else is said on the matter.
