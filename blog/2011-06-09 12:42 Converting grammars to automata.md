Wow, ages since I last wrote a blog post. April, in fact. Well, I have today been working on something that has the potential for fun! I have been making a little Python script using pygraphviz (which is wonderful) to convert formal grammars into automata. Currently it can only do regular (in right-regular form) and context-free (in CNF) grammars, but I want to add support for more.

Here is an example of the sort of glorious pictures you can generate:

    right-regular
    S → aA | bB
    A → aC | bA
    B → aS | bA
    C → aB | λ    

![Finite State Automata](files/rightregular.png)

    context-free
    S → AC | BD | λ
    A → 0
    B → 1
    C → SA
    D → SB

![Nondeterministic Pushdown Automata](files/contextfree.png)

Pushdown automata generated accept by empty stack.

You can see the code, and my to-do feature list, on [github](https://github.com/Barrucadu/Automatool).
