---
title: A Gentle Introduction to Parsec
description: Introduction to Parsec by constructing a parser for CSV files.
---

It seems to me that there aren't many step-by-step introductions to
parsec, where you build up a parser as you go. This is especially the
case for applicative parsec, which is a shame as applicative functors
are nice. So, I wrote one. Today, we are going to learn how to use
applicatives and parsec to parse a CSV file. We'll start off with a
very basic one where there can be no commas or escape characters in
the fields, then add support for quoted fields which can contain any
character, and then we'll add support for special escape characters
(numeric literals and the like). Finally, I'll leave two small
exercises that you might want to work on, just to check that you
managed to get everything.

## The Basic Parser

~~~~{.haskell}
import Text.Parsec
import Control.Applicative ((<$), (<*), (*>), liftA)
import Data.Char           (chr)
 
parseCSV :: String -> Either ParseError [[String]]
parseCSV = ...
~~~~

Great things have small beginnings. Parsec is based upon *parser
combinators*: there are a few basic parsers which do things like match
a character, and then everything else is built by combining them. A
*combinator* (or supercombinator) is a function which is composed
entirely out of bound variables and other
(super)combinators. Combinators are nice, as they compose easily, and
so building up very complex operations out of combinators is often
very easy. Furthermore, it is not unusual for a function built out of
combinators to not refer to any variables at all, and so combinators
naturally express the notion of data flow programming, where you say
how you want the data to be manipulated, but don't really talk about
the actual data.

In order to be able to work like this, parsec has two interfaces:
monadic, and applicative. The monadic way is the traditional way to do
it, but it leads to a very imperative style of writing parsers, which
doesn't look very nice, or even look very much like you're using
combinators at all. The applicative way is the newer way and, whilst
not as powerful as the monadic way, allows you to parse any context
free grammar. In fact, as Haskell is lazy, you can actually parse any
context sensitive grammar with a finite alphabet, but that's a bit of
a hack: in general, applicative functors are not powerful enough to
parse context sensitive grammars.

Look at the type signature of our `parseCSV`, it returns an `Either
ParseError [[String]]`. Parsec uses `ParseError` when things go wrong,
and tells you what it actually found and what it expected to find,
although there are functions which allow you to manually set an error
message, rather than use the default one. The success type is a list
of lists of strings: I decided to represent a parsed CSV file as a
list of rows, where each row is a list of string fields. Now, let's
see what we can do:

~~~~{.haskell}
parseCSV = parse csvp ""
~~~~

See, no variables! What's this doing? Well, the parse function runs a
parser (`csvp`) with a given source name (`""`, can be used in error
messages), on a given input. So, I hear you cry, if `csvp` is the
parser, that's where all the hairiness is, right? Actually, no,

~~~~{.haskell}
csvp :: Parsec String () [[String]]
csvp = line `endBy` newline <* eof
~~~~

Still pretty simple, but we have some more notation to explain. The
type `Parsec String () [[String]]` means that this is a parser which
takes an input of type `String`, some state of type `()` (so no state,
in our case), and produces an output of type `[[String]]`. `line` is
another parser I've defined, which handles each line of our
input. `endBy p s` is a built-in combinator, which repeatedly runs the
parser `p`, collecting its results in a list, where each `p` is
followed by a separator `s`, in this case the separator is newline,
which is a built-in parser to match---you guessed it---newlines. `eof`
is another built in parser, I'm sure you can guess what it does.

Let's spend a little time talking about `(<*)` and its brothers. Here,
we get our first taste of applicative functors! The type of `(<*)` is
`Applicative f => f a -> f b -> f a`, and `p <* q` can be read as "do
`p`, then do `q`, and only return the result of `p`". Thus, it is a
combinator for gathering side-effects: in this case, parsing some of
the string. As you might expect, there is a `(*>)`, which throws away
the result of the left parser. The other combinator I'm going to make
use of is `(<$) :: Applicative f => a -> f b -> f a`, where `x <$ p`
can be read as "do `p`, and then return the value `x`".

Ok, let's continue. `parseCSV` is nice, `csvp` is nice, surely `line`
must be horrible!

~~~~{.haskell}
line :: Parsec String () [String]
line = cell `sepBy1` char ','
~~~~

Oh. Well, we see another custom parser here, `cell`, which parses the
contents of an individual cell. We also get a new combinator,
`sepBy1`, and a new basic parser, `char`. `sepBy1 p s` parses `p`s
separated by `s`s, much like `endBy`, except the final `p` doesn't
need an `s` after it, also, the 1 in the name means that there must be
at least one `p`. Or, in terms of CSV files, each row has at least one
field. As you can probably guess, the parser `char c` succeeds if it
can parse the char, `c`.

Are we nearly at the end of this surprisingly easy to navigate rabbit
warren of parsers? Yup, just one more to go. Truly, *this* must be
where the horror lurks!

~~~~{.haskell}
cell :: Parsec String () String
cell = many $ noneOf ",\n"
~~~~

And that's it, a parser for CSV files. `many p` runs the parser `p` as
many times as it can, collecting the results, `noneOf cs` is a parser
which matches any char not in `cs`.

Well, this is all well and good, but the astute reader will have
noticed this doesn't let us have a comma inside a field, and may also
have noticed that I haven't used all of the things I imported way back
at the beginning. We need to go deeper.

## Quoted Cells

It's fairly common to allow special characters like commas in the
fields of a CSV file by quoting the entire field. Thus, your data may
look like this: `foo,bar,"baz, bat"`. How can we do this? Well, let's
break it down into cases.

1. A cell is not quoted, in which case it cannot contain commas or
   newlines.

2. A cell is quoted, in which case it can contain commas and newlines.

But now we surely have another problem, how do we embed a literal
quote into our quoted fields? Let's adopt the convention `""`
represents a single quote, if it appears inside a quoted field. For
example, `"bar said ""hello, world"" to foo."` This seems to work.

As I broke the problem into two cases, you may have guessed where this
is going. The choice combinator! `p <|> q` executes `p` and, if `p`
fails without consuming any input, executes `q`. This is sometimes a
bit restrictive, and so the parser `try p` executes `p`, and if `p`
fails it acts like it hasn't consumed any input. Let's build up our
parser.

~~~~{.haskell}
cell :: Parsec String () String
cell = cell' <|> many (noneOf ",\n")
~~~~

Same as before, except we have some `cell'` parser, which parses a
quoted cell. The distinguishing feature of a quoted cell is that it's
between two quotes. Fortunately, parsec has a `between l r p`
combinator, which parses a `p` between an `l` and an `r`.

~~~~{.haskell}
where cell' = between (char '"') (char '"') $ many (noneOf "\"")
~~~~

Nearly there, we just need to add the `""` to `"` code now. Another
choice.

~~~~{.haskell}
where cell' = between (char '"') (char '"') $ many chr
      chr   = noneOf "\"" <|> try ('"' <$ string "\"\"")
~~~~

And now the previously mentioned `(<$)` combinator comes in to play. I
don't really like it, as it does things in the opposite order to how
they are specified, so I'll be changing it for something else
later. But here, it is ok.

We can now parse CSV filed to our hearts content, and there are no
obvious problems with our parser. However, we could make it
shinier. Let's add support for special characters (eg, "`\a`") and
numeric literals (eg, "56"`).

## Specials and Literals

You may be tempted to write something like `literal <|> specialchar
<|> noneOf "\""`, using your new-found knowledge of `(<|>)` to choose
between all of the possibilities, and that would not work. Both our
literals and our special characters start with a backslash, and so if
literal consumes a backslash, and then fails to progress (as it's a
special character, not a literal), the whole chain will fail, as
`(<|>)` only tries the next parser if no input was consumed. We can
either work around this by rearranging our grammar, or by using
`try`. I went for that, as it's easier:

~~~~{.haskell}
cell :: Parsec String () String
cell = cell' <|> many (chr ",\n\\")
    where cell'   = between (char '"') (char '"') $ many (chr "\\\"")
          chr bad = try literal <|> specialchar <|> noneOf bad
~~~~

Now, don't rush off and implement `literal` and `specialchar` yet!
Let's think about types. Haskell is all about types, and getting your
types right is a big help in checking your program is right. Clearly,
they both need to consume a `String` and produce a `Char`. Why? Well,
`noneOf` does, and the types have to match. Alternatively, a special
character (or literal) is a multi-character sequence, like "`\0`",
it's a minimum of two characters long. Clearly we can't consume less
than that and still match it. Furthermore, both literals and specials
express a single character, so that is clearly what should be
returned.

Let's also think about how we can go about parsing them, before we
head off and do it. An approach that could work would be to match a
backslash, then dispatch on the next character to decide what to
do. Special characters are of the form "`\x`" for some `x`, and literals
are of the form "`\xx`", for some `x`. Furthermore, the type of digits
after a literal depend on the base. Let's simplify things and only
consider hexadecimal and octal (as parsec has parsers for those). So,
for specials we want to map a char to a char, and for literals we want
to map a char to a parser. How about these?

~~~~{.haskell}
specialCharacters :: [(Char, Char)]
specialCharacters = [('0', '\0'), ('a', '\a'), ('b', '\b'), ('f', '\f'),
                     ('n', '\n'), ('r', '\r'), ('t', '\t'), ('v', '\v'),
                     ('"', '"'), ('\'', '\''), ('\\', '\\')]

literalNumbers :: [(Char, Parsec String () Char)]
literalNumbers = [('x', hexDigit), ('o', octDigit)]
~~~~

I've left out decimals, as there isn't really a traditional prefix for
those, so it just complicates matters at this stage (guess what one of
the exercises will be). Before progressing, have a think about how to
implement `specialchar`. It's a parser which matches a string and
returns the appropriate character, or fails if none match. We can
explicitly fail by returning `parserZero`, the parser which fails
without consuming any of its input.

Well, here's my solution:

~~~~{.haskell}
specialchar :: Parsec String () Char
specialchar = char '\\' *> special' specialCharacters
    where special' ((esc, c):cs) = char esc *> parserReturn c <|> special' cs
          special' [] = parserZero
~~~~

I could have written it as a `foldr`, but decided to make it explicit,
so it's simple. I also used `parserReturn`, where `p *> parserReturn
c` ≡ `c <$ p`. I think it's just easier to read, as the flow of data
isn't backwards.

Having seen `specialchar`, you should be able to implement `literal`,
as it's almost the same. The only difference is that instead of
returning straight away, we parse some more and do something with the
result (specifically, convert it from a string of digits into a
char). Here you go,

~~~~{.haskell}
literal :: Parsec String () Char
literal = char '\\' *> literal' literalNumbers
    where literal' ((c, f):cs) = char c *> tochar c (many1 f) <|> literal' cs
          literal' [] = parserZero
          tochar c    = liftA $ \s -> chr . read $ '0' : c : s
~~~~

And that is it, our parser is complete. I hope I explained everything
clearly enough, and you now feel comfortable with the basics of
applicative parsec. The parsec haddock is really the best place to
look for documentation; it's guaranteed to be always up to date and
complete.

## Exercises

If you want to practice your understanding, there are two main
problems I have with this parser: escape sequences are only one
character long, and there are no decimal literals. It is your task,
should you choose to accept it, to implement these features! More
precisely,

- Allow special characters like "`\SOH`", "`\BS`", and "`\DEL`". See
  [Real World Haskell][E1] for a decent list.

- Allow decimal literals in the form "`\123`", a sequence of decimal
  digits immediately after a backslash character with no prefix
  character.

[E1]: http://book.realworldhaskell.org/read/characters-strings-and-escaping-rules.html

## Further Reading

- [Parsec Documentation][F1]
- [Real World Haskell][F2] (particularly chapter 16)
- [Write Yourself a Scheme in 45 Hours][F3] (particularly the section on parsing)

[F1]: http://hackage.haskell.org/package/parsec-3.1.3
[F2]: http://book.realworldhaskell.org/
[F3]: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
