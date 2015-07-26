---
title: Weekend Project: Writing a Gopher Server in Erlang
description: How better to learn Erlang than by writing a server?
---

For a little while now I've wanted to learn Erlang, so I finally
borrowed a copy of [Programming Erlang][] (although mine is the first
edition) and set to work. There's also [Learn You Some Erlang][lyse]
as a free online resource, in the style of
[Learn You A Haskell][lyah].

For a similar amount of time, I'd wanted to write a [Gopher][]
server. Gopher is a really simple protocol for menu-based document
retrieval: a client connects, and sends a string to the server. The
server then responds with the resource associated with that string,
which will typically be a file or a directory listing, and closes the
connection.

You can see what it's like with [this proxy][proxy] if you don't have
a Gopher client. Lynx can browse Gopher, and there is the Overbite
plugin for Firefox (both using the `gopher://` URI scheme). Have a
look at [gopher://gopherproject.org/][gopherproject] for a fairly
fully-featured Gopher site.

My working server is called lemon, is [available on GitHub][source],
and is running on this server right now serving a Gopher version of my
website!

[Programming Erlang]: https://pragprog.com/book/jaerlang2/programming-erlang
[lyse]:               http://learnyousomeerlang.com/
[lyah]:               http://learnyouahaskell.com/
[Gopher]:             https://www.ietf.org/rfc/rfc1436.txt
[proxy]:              http://gopher.floodgap.com/gopher/
[gopherproject]:      gopher://gopherproject.org/
[source]:             https://github.com/barrucadu/lemon

A Basic Network Server
----------------------

First thing's first, I wanted to be able to open a listening socket,
and fork off a new process to handle each connection.

~~~~{.erlang}
start() -> start(70)

start(Port) ->
    Pid = spawn(fun() ->
        {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}]),
        serve(Listen)
    end),

    {ok, Pid}.
~~~~

Erlang allows functions of the same name but different arities, and
these are *totally separate things*. So if a function has some
sensible default arguments, you can make a lower-arity version which
fills them in. In the code above, the two functions would be referred
to in export/import statements as `start/0` and `start/1`, after their
arities. Things starting with a lower-case letter are symbols,
variables start with upper-case letters.

So this spawns a new thread which opens a listening socket on the
given port, defaulting to 70. The `{ok, Listen}` is a *pattern match*
on the return value of `gen_tcp:listen`. If that fails to open the
socket (something else is using that port, for example) the pattern
match will fail, and an error will be reported in the console.

The socket is opened in `binary` mode, and the `{active, false}` means
that it doesn't deliver messages received to the socket as Erlang IPC
messages to the process which owns it.

Finally, that thread starts the server, and the function returns the
process ID of the spawned thread.

~~~~{.erlang}
serve(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Sock} ->
            Pid = spawn(fun() -> responder(Sock) end),
            gen_tcp:controlling_process(Sock, Pid);
        _ -> ignored
    end,

    serve(Listen).

responder(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Msg} -> io:format(Msg);
        _ -> ignored
    end,

    gen_tcp:close(Sock).
~~~~

Here, the `serve` function blocks until there is a connection, which
it passes off to another process to handle. This other process
currently just reads from it (the "0" means it reads all it can),
prints it to stdout, and closes the socket.

In Erlang, sockets have a notion of a *controlling process*, which is
the process that is allowed to read from, write to, and close the
socket. This means you can pass sockets around between processes and
be happy that you know exactly which process is using it at a time.

Gophering It Up
---------------

Implementing a basic Gopher server is really easy. It took me
[4 commits][], never having dealt with the language or the protocol
beforehand.

Here's the now mostly-complete `responder` function:

~~~~{.erlang}
responder(Sock, Config) ->
    case gen_tcp:recv(Sock, 0) of
      {ok, Msg} ->
          StrMsg  = binary_to_list(Msg),
          Decoded = lists:takewhile(fun(C) -> C =/= $\r end, StrMsg),

          case check_path(string:tokens(Decoded, "/")) of
              {ok, Path} ->
                  FullPath = proplists:get_value(fileroot, Config) ++ "/" ++ Path,
                  case file:read_file_info(FullPath) of
                      {ok, #file_info{type=directory}} ->
                          serve_directory(Sock, Config, FullPath);
                      {ok, #file_info{type=regular}}   ->
                          serve_file(Sock, Config, FullPath);
                      {ok,    _} ->
                          serve_file_error(Sock, Config, FullPath, "cannot be served");
                      {error, _} ->
                          serve_file_error(Sock, Config, FullPath, "does not exist")
                  end;

              error ->
                  serve_error(Sock, Config, "invalid path")
          end;
      {error, closed} ->
          serve_error(Sock, Config, "??? bad request")
    end,

    gen_tcp:close(Sock).
~~~~

For a basic Gopher server, simply serving the filesystem, here is what
you need:

1. A root directory to serve from.
2. A function to check if a path is a file, directory, or missing.
3. A function to, given a directory path, produce and serve a directory listing.
4. A function to, given a file path, serve the contents of the file.

Other desirables are functions to check that the path the client sends
you doesn't contain any hidden files (to prevent escaping from the
served directory) and to serve error messages.

A Gopher directory listing is something like this (taken from the RFC):

~~~~
0About internet Gopher	Stuff:About us	rawBits.micro.umn.edu	70
1Around University of Minnesota	Z,5692,AUM	underdog.micro.umn.edu	70
1Microcomputer News & Prices	Prices/	pserver.bookstore.umn.edu	70
1Courses, Schedules, Calendars		events.ais.umn.edu	9120
1Student-Staff Directories		uinfo.ais.umn.edu	70
1Departmental Publications	Stuff:DP:	rawBits.micro.umn.edu	70
.
~~~~

Each line consists of a single-character type and a name, a tab
character, the path corresponding to that resource (i.e., what the
client should send to the server), a tab character, the server
providing the resource, another tab character, and the port. The whole
thing is terminated with a full stop on a line by itself.

As the server and port for each entry are specified, this allows
Gopher servers to seamlessly combine local and remote resources with
ease. It's certainly a nice way to disseminate information.

[4 commits]: https://github.com/barrucadu/lemon/commit/21970cbd47602b9e2d57f707ce58556e0adc168b

Going Further
-------------

Basic Gopher is indeed easy to implement, but usually you would want
the option to have more than just directory listings. One of the types
is "i", for "information", and is used to embed data into menus. This
means we need something more than just listing the requested
directory.

I found a pre-existing format for this, called [link files][], where
you can have a `.Link` file in a directory with entries in this
format:

~~~~
Name=Super-Dimensional Fortress Gopher
Type=1
Path=/
Host=freeshell.org
Port=70
~~~~

All of the fields other than the name are optional (with the type
defaulting to "i"). However, it's a bit verbose if you have multiple
information entries in a row, so I added a format almost the same a
directory listing, called "menu files", which live in `.Menu`:

~~~~
{
 Welcome to the HackSoc Gopher Server!
This server kindly provided by Bytemark

}

1Gopher Project		gopherproject.org	70

{
---------------------------------------

}

1User Directories	users	runciman.hacksoc.org	70
0README	README	runciman.hacksoc.org	70
~~~~

This is like a regular directory listing, except that the trailing
full stop is not required, and every line (including blank lines)
enclosed between curly braces is sent as an information line. This is
rather concise compared to the link format, at the cost of being
easier to get wrong.

[link files]: http://aftershock.sourceforge.net/manual-1.01.txt

What about the IPC?
-------------------

Cheap easy IPC is the major selling point of Erlang, and sadly I
haven't made any use of it at all so far! Furthermore, there is an
ugly wart in the implementation: there's no way to *gracefully*
terminate it, short of terminating the entire Erlang process!

I see a happy solution.

~~~~{.erlang}
gopher(Listen, Config) ->
    receive
        _ -> gen_tcp:close(Listen), exit(terminated)
    after 0 ->
        case gen_tcp:accept(Listen, 100) of
            {ok, Sock} ->
                Pid = spawn(fun() -> responder(Sock, Config) end),
                gen_tcp:controlling_process(Sock, Pid);

            _ -> ignored
        end
    end,

    gopher(Listen, Config).
~~~~

The `receive`/`after`/`end` block is half of how Erlang does IPC. A
`receive` is much like a `case`, it specifies a bunch of patterns
(optionally with boolean guards attached), and executes the first
matching pattern. The major difference is that `receive` blocks until
a message is received. The `after` specifies a timeout, after which
some code can be executed. However, *if there are messages waiting to
be processed*, the timeout *will not fire* until all matching messages
have been handled!

The effect of this is to make the main loop behave like so: if there
is a message waiting, immediately close the socket and
terminate. Otherwise, block until either a connection is received or a
100ms timer (that's the second parameter to `gen_tcp:accept`)
passes. If a connection is received, fork off a thread to deal with
it. Loop.

The second half of how Erlang does IPC is sending messages:

~~~~{.erlang}
stop(Pid) -> Pid ! stop.
~~~~

Actually, as `gopher` don't check what the message contains, we could
send anything, but I thought `stop` was clearest.

Not quite at the level of "thousands of frequently-communicating
processes all happening in parallel distributed across multiple
machines" that Erlang is touted for, but it's my first go.

Retrospective
-------------

I quite liked the little of Erlang I had to use to implement this, and
the weird syntax is now seeming a little less alien. I'm also now of
the opinion that a Gopher server is a good project to get your feet
wet with a new language: it's a small and simple protocol, yet
implementing a server requires:

- File(system) access
- IPC
- Sockets
- String/list processing
- Threading

Definitely better than a "Hello, world!"
