This post is inspired by the following Arch Linux BBS thread, [C error handling](https://bbs.archlinux.org/viewtopic.php?pid=998487#p998487).

I have a near-diametrically opposite viewpoint when it comes to error handling with one of my housemates (along with many other aspects of programming). To summarise, user errors should be handled gracefully, whereas you shouldn't even attempt that with logic errors. Have a nice ASCII art picture to demonstrate what I consider the classes of errors to be:

    Recoverable:
                         [user errors]
    Non-recoverable:
                   [logic errors][segfaults]
            [library problems][libc error][os error]

As you can see, what I consider to be a recoverable error is rather limited, I shall now explain why I have this viewpoint.

### On the non-recoverability of logic errors

Logic errors, like segfaults (which generally are a symptom of a logic error), indicate a bug in the program code, they "shouldn't" occur. Let's take an example, you're adding data to a linked list. You call malloc, it fails. How can you possibly recover from that? You can't store the thing you need to store, what sort of possible graceful recovery is there from that? Similarly, you call a function to convert from a larger integer type into a smaller type, and you give it a value too big - what can it do? I think the only thing to do in those situations is to fail as soon as it goes wrong, and have the programmer actually fix the bugs rather than introduce handling code to merely work around them.

Of course, I'm not saying errors should just run rampant, that could make them harder to track down - assert method pre- and post-conditions to check all is in order, and then you will know exactly where it is failing.

### User errors, however...

Errors which the user could have caused - configuration file syntax, bad filename, bad IP address, any errors arising from what the user has done, should be handled gracefully. By "gracefully" in this context, I mean "an error message describing the problem should be displayed, and clean-up should be done". If the user has caused the problem, unless they're also a developer, an assertion failing isn't going to help with their problem.

As an example, if I asked the user to enter a number and they gave a string, I would repeat the prompt until a number was entered.

### But what about libraries?

I have not written any libraries that are not for my personal use, and so I would possibly adopt a different policy if working on a larger system. I still maintain that logic errors are bugs and so shouldn't be worked around, but I can see the need for that to happen in some cases, where high reliability is essential and dying over a mere inability to allocate memory is not acceptable.

### An example

Below are two memory management methods, with some helper macros, that I use. The first allocates some memory and zeroes it (calloc does all that in one fell swoop), and then asserts that it has been allocated (postcondition check). The second frees a pointer, after asserting that it is not null (precondition check), and then sets the pointer to NULL, to prevent multiple frees of the same pointer.

#### utils/memory.h:
    #ifndef __SHARED_MEMORY_H
    #define __SHARED_MEMORY_H
    
    #define xalloc(T)     (T*) xalloc_internal (sizeof (T))
    #define xallocn(T, N) (T*) xalloc_internal (sizeof (T) * (size_t)N)
    #define xfree(P)      xfree_internal((void**)&P)
    
    /**
     * Allocate, check, and zero some memory
     *
     * @param The size of memory to allocate
     */
    void*
    xalloc_internal (size_t size);
    
    /**
     * Free a pointer and set it to NULL
     *
     * @param Pointer to the pointer to free
     */
    void
    xfree_internal (void** pointer);
    
    #endif

#### utils/memory.c
    #include <stdlib.h>
    #include <assert.h>
    #include "memory.h"
    
    /**
     * Allocate, check, and zero some memory
     */
    void*
    xalloc_internal (size_t size)
    {
      void *out;
    
      out = calloc (1, size);
    
      assert (out != NULL);
    
      return out;
    }
    
    /**
     * Free a pointer and set it to NULL
     */
    void
    xfree_internal (void** pointer)
    {
      assert (pointer  != NULL);
      assert (*pointer != NULL);
    
      free (*pointer);
    
      *pointer = NULL;
    }
