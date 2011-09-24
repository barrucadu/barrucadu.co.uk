Lately I've been working through the recommended reading list for computer science at York and, naturally, Turing machines get mentioned in these books, being *the* computing machines. Today, something finally clicked and I understood how they work and how to program them. The result is a Python module for constructing and using Turing machines.

The module can be found [on my github account, along with all my other stuff](http://github.com/Barrucadu/home/blob/master/code/python/turing/turing.py).

Before writing this module I designed a simple Turing machine on paper to increment and decrement binary numbers an arbitrary number of times to calculate simple addition and subtraction problems. For example:

    Input tape:   <10>+++-#
    Output tape:  <100>////#

What has been calculated there is **2 + 3 - 1**, and you can see it has found the correct answer of 4. I shall firstly explain how this particular Turing machine works, then I'll explain how it was implemented using my module.

### The Machine

The machine, hereinafter referred to as *M* increments and decrements an input binary number, *N*, as dictated by a series of +'s and -'s. *M* uses an alphabet of 8 symbols, plus a special *blank symbol*, which fills every cell not occupied by another symbol (as is the case for all Turing machines). It is assumed that the tape extends infinitely to the left and the right of the input expression, that *M* starts with the read/write mechanism pointing at the cell containing the **>** symbol, and that *M* starts in the **ready** state.

#### Alphabet

The alphabet used by *M* is as follows:

    0 = binary zero
    1 = binary one
    > = lower end of number
    < = upper end of number
    + = increment number
    - = decrement number
    / = no operation
    # = halt

The alphabet could actually be contracted, as the **<** and **>** symbols are not required (only being present for convenience), and could be replaced by a few more state transitions. In fact, the **#** and possibly **/** could also be removed, reducing to an alphabet of four symbols. Additionally, if I chose to represent numbers in unary rather than binary, an alphabet of three symbols could therefore be used. However, for now, I shall use the eight-symbol variant.

#### State transitions

The state transitions are the key to the machine, the following diagram shows how the machine behaves in each state for every symbol it encounters in that state. This diagram could perhaps be contracted, though I have not tried.

(NOTE: The state transition diagram has since been lost, and I unfortunately no longer have the code to regenerate it.)

As you can see, if a **<** symbol is reached before a **1** when decrementing *N* (ie: subtracting 1 from 0), the machine halts. This machine only works for positive integer values of *N* (and zero, when incrementing).

Just a note on the notation used in the diagram for state transformations, "A/B,C" is used, where **A** corresponds to the symbol read, **B** corresponds to the symbol written, and **C** corresponds to the direction moved. A downside of using a slash as a separator between **A** and **B** means that, when a slash is read and written, the string "///" appears. Additionally, if **A** is missing (as is the case when moving from the *grow* state to the *reset* state), the symbol read is the blank symbol.

#### Knowing alphabet and transitions

It is trivial to see how it works. Here is an example showing **1 + 1 - 2= 0**.

    <1>+--#     symbol read '>', state 'ready'
    <1>+--#     symbol read '+', state 'ready'
    <1>/--#     symbol read '>', state 'inc'
    <1>/--#     symbol read '1', state 'inc'
    <0>/--#     symbol read '<', state 'inc'
     10>/--#    symbol read ' ', state 'grow'
    <10>/--#    symbol read '1', state 'reset'
    <10>/--#    symbol read '0', state 'reset'
    <10>/--#    symbol read '>', state 'reset'
    <10>/--#    symbol read '/', state 'ready'
    <10>/--#    symbol read '-', state 'ready'
    <10>//-#    symbol read '/', state 'dec'
    <10>//-#    symbol read '>', state 'dec'
    <10>//-#    symbol read '0', state 'dec'
    <11>//-#    symbol read '1', state 'dec'
    <01>//-#    symbol read '1', state 'reset'
    <01>//-#    symbol read '>', state 'reset'
    <01>//-#    symbol read '/', state 'ready'
    <01>//-#    symbol read '/', state 'ready'
    <01>//-#    symbol read '-', state 'ready'
    <01>///#    symbol read '/', state 'dec'
    <01>///#    symbol read '/', state 'dec'
    <01>///#    symbol read '>', state 'dec'
    <01>///#    symbol read '1', state 'dec'
    <00>///#    symbol read '>', state 'reset'
    <00>///#    symbol read '/', state 'ready'
    <00>///#    symbol read '/', state 'ready'
    <00>///#    symbol read '/', state 'ready'
    <00>///#    symbol read '#', state 'ready'

However, even for such a simple calculation, 29 steps are required.

### The Implementation

My class does most of the work, so all that needs to be done is to provide information about the desired machine, and provide an input. The basic code to create and run a Turing machine is as follows:

    from turing import *
    
    M = Turing(tape, states, haltstates, initialstate, transitions, alphabet, tapepos, blanksymbol)
    M.run()
    
    # At this point, the finished tape can be read:
    print(M.tape)

#### Turing() parameters

Many of the parameters are fairly self-descriptive, however I will explain all of them:

 * **tape** = an input tape to the machine, given as a string. This will be expanded to the left and right as required.
 * **states** = a list of state names that the machine can be in.
 * **haltstates** = a list of those states in **states** which should cause the machine to halt.
 * **initialstate** = the name of the initial state of the machine.
 * **transitions** = a dictionary showing state transitions (explained below).
 * **alphabet** = a list of allowed symbols for this machine.
 * **tapepos** = the starting tape position, given as a string index for **tape** (eg, **tapepos** = 0 corresponds to the first entry in **tape**).
 * **blanksymbol** = the symbol used to fill newly-added cells in the tape, must be one character, by default set to a space. If not in **alphabet**, it will be added automatically.

Now, for the transitions dictionary. This consists of entries in the forms:

    "state-symbol" : ["newsymbol", "newstate", "direction"],
    "state-"       : ["newsymbol", "newstate", "direction"]

Where the **state-symbol** syntax matches that symbol in the given state, and the **state-** syntax matches the blank symbol in the given state. **newsymbol** is the symbol to replace the current one on the tape with, **newstate** is the name of the state to enter, and **direction** is the direction to move in ("R" or "L").

#### Implementation of incdec

You are now fully armed to use my Turing class, and so I give you the example of my incdec Turing machine. The machine definition section which follows is the most interesting, particularly the state transition dictionary which, you should check for yourself, contains all of the transitions described in the previous diagram:

    states      = ["ready", "inc", "dec", "grow", "reset", "halt"]
    haltstates  = ["halt"]
    alphabet    = ["0", "1", "<", ">", "+", "-", "/", "#"]
    transitions = {"ready->" : [">", "ready", "R"],
                   "ready-/" : ["/", "ready", "R"],
                   "ready-#" : ["#", "halt",  "L"],
                   "ready-+" : ["/", "inc",   "L"],
                   "ready--" : ["/", "dec",   "L"],
    
                   "inc->"   : [">", "inc",   "L"],
                   "inc-<"   : ["1", "grow",  "L"],
                   "inc-/"   : ["/", "inc",   "L"],
                   "inc-0"   : ["1", "reset", "R"],
                   "inc-1"   : ["0", "inc",   "L"],
    
                   "dec->"   : [">", "dec",   "L"],
                   "dec-<"   : ["<", "halt",  "R"],
                   "dec-/"   : ["/", "dec",   "L"],
                   "dec-0"   : ["1", "dec",   "L"],
                   "dec-1"   : ["0", "reset", "R"],
    
                   "grow-"   : ["<", "reset", "R"],
    
                   "reset-0" : ["0", "reset", "R"],
                   "reset-1" : ["1", "reset", "R"],
                   "reset->" : [">", "ready", "R"]}

After the machine definition, the rest of the code is trivial in comparison:
    
    tape = "<1>+--#"
    
    incdec = Turing(tape, states, haltstates, "ready", transitions, alphabet, tape.index(">"))
    incdec.run()
    
    print("Input tape:   " + tape)
    print("Output tape:  " + incdec.tape)
    print("Last state:   " + incdec.states[incdec.mystate])
    print("Symbols read: " + str(incdec.symbols))

As a side effect of this whole endeavour, I have proven to myself that Python 3 is a Turing-complete language :)
