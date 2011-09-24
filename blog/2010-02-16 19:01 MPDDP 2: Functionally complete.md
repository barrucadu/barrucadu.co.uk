That's right, MPDDP 2 is now functionally complete, it can do everything that regular MPDDP can, and more. The downside is that, currently, it's incredibly slow to start up, but I'm thinking of a solution for that.The whole routine for checking if a track is allowed by the rule is fairly inefficient currently, so that'll likely be rewritten completely soon and, for ease of parsing, I might change the rule syntax to S-expressions. Perhaps a MPDDP 2.1 already?

### Barrucadu's Guide to MPDDP Version Numbers

Which number has changed?

**X**.y.z
: everything has changed completely. Your configuration file definitely won't work.

x.**Y**.z
: something *big* has changed more-or-less completely. Your configuration file probably won't work

x.y.**Z**
: something has changed, but not really that much; probably just a bugfix. Your configuration file should be fine.

### Configuration

MPDDP 2 has a completely different configuration syntax to MPDDP, here's an example:

#### MPDDP

    server = localhost
    path:fred/

Connect to the MPD server at localhost and use all tracks in the path fred/.

#### MPDDP 2

    rule fred {
        path ~= "fred/"
    }
    host localhost
    active fred

Connect to the MPD server at localhost, and use the rule fred, which matches all tracks with a path containing fred/.

While these two examples are functionally equivalent, the MPDDP 2 example is longer. In fact, my MPDDP 2 config file is a lot longer than my MPDDP config file, but that's because currently it's a direct translation of my MPDDP config file to MPDDP 2 syntax, and I'm not using many MPDDP 2 specific features yet. However, MPDDP 2 can do things that regular MPDDP can't, or can only do using the MPDSPL integration (which has been removed from MPDDP 2). Consider the following, for example:

    rule firstfred {
        artist = "fred"
        album = "first"
    }

A rule which matches all tracks by "fred" in the album "first", or the following:

    rule fg {
        artist = "fred" or \
        artist = "george"
    }

A rule which matches all tracks by "fred" or "george". Also, note that I split the line into two by putting a backslash before the newline. You can escape newlines like that anywhere in the config file. I suppose that's enough examples for now, now I shall go into more detail about how the rules work.

### Rules

Each rule is more correctly known as a ruleset. Each ruleset is broken up into several rules (one line = one rule), and each rule is broken up into several conditions (eg: 'artist = "fred"'). A track matches the ruleset if it matches *all* of the rules.

Conditions have two forms, the first takes three parameters, the second only one.

#### 'Normal' Conditions

These conditions are in the form "keyword comparison value" (or "value comparison keyword" (or even "keyword comparison keyword", or "value comparison value")), where the keyword is one of the following:

  * artist
  * album
  * title
  * path
  * file
  * genre
  * time

Comparison is one of the following:

  * = (A equals B)
  * != (A does not equal B)
  * ~= (A contains B)
  * !~= (A does not contain B)

And the value is a string contained in quotes. The following are all valid conditions:

  * album ~= artist
  * artist ~= "Fred"
  * path !~= "boring-music/"

#### 'Rule' conditions

These conditions take only one parameter, the name of a rule. These conditions evaluate to true if the track matches the given rule. These are best explained through an example:

    rule fred {
        artist = "fred"
    }
    rule fredfoo {
        rule "fred"
        album = "foo"
    }

The rule fredfoo would then match all tracks which have artist "fred" and album "foo".

#### Chaining conditions together

As I said, each rule can contain several conditions. These conditions are chained together with the boolean operations "and", "or", and "xor", for example:

  * artist = "fred" or artist = "george"
  * album = "foo" xor path ~= "bar/"

#### Putting it all together

So, now you can have rules like this, which would be impossible in MPDDP, even with the MPDSPL integration:

    rule dark {
        artist = "Nox Arcana" or \
        artist = "Mortiis"
        album != "Grimm Tales"
        title !~= "Child"
    }

A rule to return all tracks by Nox Arcana and Mortiis which do not contain "Child" in the title and are not in the album Grimm Tales.

### Configuration Continued

So, what other than the rules and a bit of syntax is different for configuration? Well, MPDDP 2 can't save the playlist, or clear the playlist when it starts, any more. I never used those features, so I removed them, and MPDDP 2 now supports password-protected MPD servers! Also, as I'm sure you noticed, the "=" is gone. I might add that back…

So, here are all the configuration options for MPDDP 2:

  * host
  * port
  * password
  * changeafter
  * playlistlen
  * update
  * active

The "active" option is a little special, it's a comma-delimited list of rules which should be used. This lets you specify many rules and, without commenting hundreds of lines out, indicate that only one or two of them should be used.

MPDDP 2 does have command-line parameters, which take priotity over the values in the config file, but I'm sure you can figure those out for yourself from the help text.
