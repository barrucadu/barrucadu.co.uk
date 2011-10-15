Over the past couple of days, I have been working on [a simple object system, based on message passing, for C](https://github.com/Barrucadu/COOP). Let's dive straight into some example code.

This is an object type:

    typedef struct
    {
      Object objstuff;
      char   *message;
    } Echo;

It consists of an "Object" (which isn't *really* an object, it just holds the stuff common to every object), and zero or more fields. In this case, we have one. But wait, where are the methods? Those are set up in the constructor, by adding message handlers to the object.

The object system is based on message passing - you send a string message, and a pointer to some arguments, to an object, and the appropriate message handler will be called (or an error signalled if there isn't one) with those arguments. A pointer to some results will be returned, if the method handler does produce anything. Let's look at an object constructor:

    /**
     * Construct a new Echo
     */
    Echo*
    Echo_construct ()
    {
      Echo *out;
    
      /* This is a macro which takes the name of the object pointer, and the type of object to make
       * it then allocates memory and does other such wizardry */
      object_make (out, Echo);
    
      /* These three lines add message handlers to the object pointer - this can be done at any time,
       * not just in the constructor.
       * If an object provides a "free" handler, it will be called when the "free" message is sent. If not,
       * default handler will be used. */
      add_message (out, "set",   &set);
      add_message (out, "print", &print);
      add_message (out, "free",  &destruct);
    
      /* This object stores a string called message */
      out->message = NULL;
    
      /* Return the new object */
      return out;
    }
    
The comments here are fairly self-explanatory. A nice thing to note is that message handlers can be added at any point, not just in the constructor, so you could make a function which takes an object and transforms it somehow, giving it extra handlers. Currently there is no way to remove or replace a message handler - any attempt to add a handler for a message that already exists will just cause an error. And now for the handlers:

    /**
     * Set the message
     */
    static void*
    set (Object *self, void *args)
    {
      Echo *this    = (Echo*) self;
      char *message = (char*) args;
    
      /* Duplicate and save the given message */
      this->message = strdup (message);
    
      return NULL;
    }
    
    /**
     * Print a message
     */
    static void*
    print (Object *self, void *args)
    {
      Echo *this = (Echo*) self;
      args = (void*) args;
    
      /* Print the saved message */
      printf (this->message);
    
      return NULL;
    }
    
    /**
     * Clean up
     */
    static void*
    destruct (Object *self, void *args)
    {
      Echo *this = (Echo*) self;
      args = (void*) args;
    
      /* Free the message */
      xfree (this->message);
    
      /* Free the object - this is done by default if there is no "free" handler. */
      object_free (this);
    
      return NULL;
    }

The "set" and "print" ones are fairly obvious, but I feel the "destruct" one needs a little explaining. The "destruct" method is bound to a message called "free" - this is a message that *all* objects implement (a default message handler is used if one isn't provided), and it frees up the memory used by the object. In the future I might construct a more elabourate runtime which supports reference-counted objects (for copying), and only calls the free handler when all references have been freed.

Now, this is all very exciting and wonderful, but it's pretty simple. How about an example with inheritance and polymorphism?

    /**
     * Construct a new Hello
     */
    Hello*
    Hello_construct ()
    {
      Hello *out;
    
      /* This is a macro which makes a new object, which is a subclass of some other object,
       * the superclass's constructor is called, and this is done so all the way up the
       * inheritance hierarchy */
      object_make_inherit (out, Hello, Echo);
    
      /* As this class does not provide a set method, the message
       * will be forwarded to the superclass */
      send_message (out, "set", "Hello, world\n");
    
      /* Now add a set method to override the superclass method */
      add_message (out, "set",   &set);
    
      return out;
    }
    
    /**
     * Do a NOP (we want the message to be *constant*!
     */
    static void*
    set (Object *self, void *args)
    {
      self = (Object*) self;
      args = (void*) args;
    
      return NULL;
    }

This is a subclass of the Echo class. It sets the message to "Hello, world\n", and then adds its own "set" handler which does nothing - this is to prevent the message from being changed. Another way to get around this would be to add a concept of private-ish messages - messages which can be called as per normal, but are not inherited. This could be something else in my more elabourate runtime.

Behold, a test!

    #include <unistd.h>
    #include <stdio.h>
    
    #include "class/hello.class.h"
    #include "runtime/message.h"
    
    int
    main (void)
    {
      Hello* obj = Hello_construct ();
    
      send_message (obj, "print", NULL);
    
      printf ("\nHandlers:\n");
      hash_table_print (obj->objstuff.super->methods);
    
      send_message (obj, "free",  NULL);
    
      return 0;
    }

This produces the most glorious output of

    Hello, world
    
    Handlers:
    Key: 'free' hash: '20' value: '0x1da1920'
    Key: 'set' hash: '98' value: '0x1da1860'
    Key: 'print' hash: '113' value: '0x1da18c0'

when executed.

So, there you have it, a very simple object system in C, based on the concept of message-passing.
