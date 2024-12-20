---
title: Advent of Ištar
template: feed.html
feed:
  image: "posts/fate-ishtar-underworld.jpg"
  image_alt: "Ištar (from Fate/Grand Order) saying \"It's the underworld!\""
  published: 2024-12-17
  summary: "This December I decided to forgo Advent of Code to participate in #ALTventOfCode with my own little project: translating the 125-line Akkadian poem The Descent of Ištar into the Netherworld."
---

<figure>
  <img src="{{ feed.image }}" alt="{{ feed.image_alt }}">
  <figcaption>{{ feed.image_alt }}</figcaption>
</figure>

<h2 style="display: none" markdown="1">Introduction</h2>

<aside markdown="1">
You've probably already figured out that I got through it rather faster than
that, in fact I posted the final chunk on the 12th, averaging around 10 lines a
day.
</aside>

I normally participate in the annual [Advent of Code][], but I always get bored
by the end of it.  This time, inspired by a friend proposing an "[Altvent of
Code][]" I decided to translate the 125-line Akkadian poem *The Descent of Ištar
into the Netherworld*, aiming for 5+ lines a day every day up to Christmas.

Ištar (the fancy "š" is pronounced "sh") was the goddess of war and sex, one of
the major gods of the Sumerian (to whom she was called Inanna) and, later,
Akkadian pantheons.  In the poem, she decides to go down to the Netherworld,
ruled over by her older sister Ereškigal, and is struck dead for her trespass,
which ends all sex on earth (there's no mention of an end to war).  She's
ultimately rescued by the intercession of Ea, the god of wisdom.

I used a few resources:

- [This Akkadian text][source1] (copied sufficiently long ago that I didn't just remember the whole thing)
- *Complete Babylonian* by Martin Worthington (isbn `9781473627802`)
- *A Grammar of Akkadian* by John Huehnergard (isbn `9781575069418`)
- *A Concise Dictionary of Akkadian* by Black, George, and Postgate (isbn `9783447042642`)
- [The Chicago Assyrian Dictionary (CAD)][cad]
- [The Babylonian Verb Conjugator (BVC)][bvc]

I translated the whole thing independently, and then afterwards compared with
two translations:

- [The one accompanying the Akkadian text I used][source1]
- [This composite text in the Cuneiform Digital Library Initiative][source2]

This is the second text I've translated, having previously done the famed
[complaint tablet to Ea-Nāṣir][eanasir] in October.

<konata:point>
If you're interested in learning more about Akkadian, check out [my Akkadian page](akkadian)!
</konata>

## Overview

Before we get into the poem entire, let's just have a brief look at the opening
lines:

> ana kurnugi qaqqari l[ā târi]<br>
> ištar mārat sîn uzunša [iškun]<br>
> iškun-ma mārat sîn uzu[nša]<br>
> ana bīti eṭê šubat ir[kalla]<br>
> ana bīti ša ēribūšu lā aṣû<br>
> ana harrāni ša alaktaša lā tayyā[rat]<br>
> ana bīti ša āšibūšu zummû nū[ra]

A couple of things stand out.

Firstly, what are all the square brackets?  Those are chunks of damaged original
text that have been reconstructed (maybe the damage only obscured the signs
rather than totally obliterating them, maybe there are context clues, maybe
there are other copies of the same text where that bit is undamaged).

The text I'm using fills in most of the gaps, but there's one line where only
the first two words are known:

> alik namtar [...]

There's not much we can do there.  I'll tend to leave out the square brackets in
the rest of this article, as they're not very interesting unless you care about
the provenance of the text.

Another thing that stands out is how repetitive and formulaic the language is.
That's because this is a poem!  Words and line breaks were chosen very
intentionally.  The [Ea-Nāṣir letter][eanasir] was challenging in places because
line breaks were just wherever the scribe needed them, but in a poem line breaks
convey useful information.

The third thing that stands out to me if that the grammar is *weird*.  More on
that later.

## How did I do?

Let's start with the score: of the 125-line poem I got 3 lines badly wrong
enough to substantially change their meaning.  I made a few other minor
mistakes, but not enough to derail things.

I think that's pretty good going for my first piece of literature!

## Translation

### Lines 1–9: The Netherworld sounds kind of like a bad place

> ana kurnugi qaqqari lā târi<br>
> ištar mārat sîn uzunša iškun<br>
> iškun-ma mārat sîn uzunša<br>
> ana bīti eṭê šubat irkalla<br>
> ana bīti ša ēribūšu lā aṣû<br>
> ana harrāni ša alaktaša lā tayyārat<br>
> ana bīti ša āšibūšu zummû nūra<br>
> ašar epru bubūssunu akalšunu ṭiṭṭu<br>
> nūra ul immarū ina eṭûti ašbū

> To the Netherworld, the land of no return,<br>
> Ištar, daughter of Sîn, set her mind, **[lit: put her ear]**<br>
> Her mind she set, the daughter of Sîn:<br>
> To the dark house, the seat of Irkalla;<br>
> To the house that none who enter can leave; **[lit: of its enterers no-departing]**<br>
> To the journey that none who take can return; **[lit of its going no-returning]**<br>
> To the house whose inhabitants are deprived of light;<br>
> Where the hungry eat food of clay, **[lit: it is necessary to feed the hungry food of clay]**<br>
> They do not see light, they dwelled in darkness.

Let's talk about grammar.  Akkadian is *usually* written in Subject Object Verb
order, with adjectives immediately following nouns, but that ordering is often
ignored in poetry.

e.g., in line 3 "iškun-ma mārat sîn uzunša": "iškun" is the verb (3rd person
singular preterite form of the verb "šakānum" meaning "to set / place / put")
whereas the subject is "mārat sîn" ("daughter-of sîn") and the object is
"uzunša" ("her ear / mind / wisdom").

The "-ma" is kind of like "and then".  It's a non-commutative emphatic marker.
So the line is emphasising "she set it" and then restating who "she" is and what
"it" is.

<konata:glasses>
The "mārat sîn" bit is using a device called "construct form": "mārat" is the
construct form of "mārtum" ("daughter").

"construct(noun1) noun2" means "noun1 of noun2" (and you can chain this with as
many nouns as you want!)
</konata>

<konata:flop>
The rules to make the construct form of a noun are weirdly complicated, based on
how many syllables the noun has, what consonants it ends with, and whether it's
singular or plural.
</konata>

### Lines 10–21: Ištar is kind of rude to some guy for no reason

> labšū-ma kīma iṣṣūri ṣubāt gappi<br>
> eli dalti u sikkūri šabuh epru<br>
> ištar ana bāb kurnugi ina kašādīša<br>
> ana atu bābi amātum izzakkar<br>
> atû mê petâ bābka<br>
> petâ bābka-ma lūruba anāku<br>
> šumma lā tapattâ bābu lā erruba anāku<br>
> amahhaṣ daltum sikkūru ašabbir<br>
> amahhaṣ sippū-ma ušbalakkat dalāti<br>
> ašabbir gišrinam-ma ašahhaṭ karra<br>
> ušellâ mītūti ikkalū balṭūti<br>
> eli balṭūti ima’’idū mītūti

> Clothed like a bird, with a garment of plumage,<br>
> Dust upon the door and bolt, **[lit: upon door and bolt a fullness of dust]**<br>
> Ištar arrived at the gate of the Netherworld.<br>
> She speaks a command to the keeper of the gate:<br>
> "Hey, gatekeeper!  Open your gate,<br>
> Open your gate, I will enter!<br>
> If you do not open the gate and I, myself, do not enter:<br>
> I will strike the door, I will break the lock!<br>
> I will smash the lintel, and I will yank the doors!<br>
> I will break the mechanism, I will remove the knob!<br>
> I will bring up the dead so they eat the living:<br>
> The dead will outnumber the living!"

I made my first big mistake here.  I translated "labšū" in line 10 as "she
is/was clothed" and so assumed it was referring to Ištar and that goddesses
often go around in bird costumes; but it should actually have been "they
[masculine, plural] are/were clothed", so this line is referring to *the
denizens of the Netherworld* from the previous lines.

<konata:glasses>
"labšū" is a form of the verb "labāšum" in what's called the "stative tense".
The stative is kind of what the name implies, it's the tense you use to refer to
*being in the state of having had the verb done to you*.  So with "labāšum"
meaning "to clothe oneself [with something]", the stative means "to be clothed
[with something]".

Check it out in [BVC](https://www.gilgamesh.ch/bvc/bvc.html?&stem=G&verbpattern=3&verbroot=484&verbrootx=lab%C4%81cum)!
</konata>

I made a smaller mistake as well, "petâ" in line 14 specifically means "open for
me", not just "open".  But that's a minor error that doesn't really change the
meaning of the line.

The final two lines are very famous in Akkadian literature:

> I will bring up the dead so they eat the living: the dead will outnumber the living!

This phrase is quoted verbatim in lots of other texts—including Gilgameš—it's
kind of Ištar's standard threat.

<konata:point>
There's some fun wordplay in this section.

The verb "kašādum" means both "to arrive" and "to conquer", so another
interpretation of line 12 is "Ištar conquered the gate of the Netherworld",
perhaps foreshadowing that she will get through the gate after all.

Another neat bit is that "atû" means "gatekeeper", and is used as such on line
14, but on line 13 the construct form "atu bābi" for "keeper of the gate" is
used instead.
</konata>

### Lines 22–28: The gatekeeper goes to get his manager

> atû pâšu īpuš-ma iqabbi<br>
> izzakkara ana rabīti ištar<br>
> izizzī bēltī lā tanaddâssi<br>
> lullik zikirki lušanni ana šarrati ereškigal<br>
> ērum-ma atû izzakkara ana ereškigal<br>
> annītu mê ahātaki ištar izzaz ina bābi<br>
> mukiltu ša keppê rabûti dālihat apsî mahar ea šarri

> The gatekeeper made ready his words,<br>
> To Great Ištar he says:<br>
> "Wait, ladyship, do not throw down the gate! **[lit: throw it down]**<br>
> Allow me to go relate your words to the queen, Ereškigal." **[lit: do your words again]**<br>
> In went the doorkeeper to speak to Ereškigal:<br>
> "Your sister Ištar stands at the gate,<br>
> The holder of the great ropes, who stirs up the cosmic waters in the presence of King Ea!"

I wasn't very confident in that last line but no, that is what it says.  It
would probably make more sense if I had a better knowledge of Akkadian
mythology.

### Lines 29–39: Ereškigal seems to have better things to do, but lets Ištar in anyway

> ereškigal annīta ina šemîša<br>
> kīma nikis bīni ēriqū pānūša<br>
> kīma šapat kunīni iṣlimā šabātūša<br>
> miná libbaša ublanni miná kabtassa ušperdânni<br>
> annītumē itti anunnakī mê ašatti<br>
> kīma aklī akkal ṭiṭṭa kīma šikāri ašattâ mê dalhūti<br>
> lubki ana eṭlūti ša ēzibū hīrēti<br>
> lubki ana ardāti ša ultu sūn hāirišina šallupāni<br>
> ana šerri lakê lubki ša ina lā ūmīšu ṭardu<br>
> alik atû pitâšši bābka<br>
> uppissi-ma kīma parṣī labirūti

> Ereškigal, in hearing this matter,<br>
> Her face became as pale as a cut tamarisk,<br>
> Her lips turned as black as the rim of a basket,<br>
> "What brought her heart to me?  What brightened her intention towards me? **[lit: "brightened her liver"]**<br>
> Do I drink water with the Anunnaki?<br>
> Do I eat clay as if it were food? Do I drink cloudy water as if it were beer?<br>
> Let me weep for young men separated from their wives.<br>
> Let me weep for young women taken captive from the lap of their husbands.<br>
> Let me weep for suckling children, gone before their day. **[lit: driven off]**<br>
> Go, gatekeeper, open your gate for her,<br>
> Treat her according to the rites of old."

I wonder if, in line 36, "taken captive" might be a metaphor for being dead -
i.e. "taken captive [by the Netherworld]".  Then you could read the three "let
me weep" lines as referring to death, which fits with Ereškigal caring about
them.

I made another small error here too, line 37 should be singular: "let me weep
for the suckling child, gone before their day".

<konata:ponder>
Something I didn't really consider when I began learning Akkadian is that it's
not just a matter of grammar and vocabulary, metaphor plays a huge part too.  So
far in this poem we've seen:

- "putting your ear" to mean directing your attention
- "lips turning dark" to indicate anger
- "brightening the liver" for feeling positively disposed to something
</konata>

### Lines 40–63: The Seven Gates of the Netherworld

> illik atû iptâšši bābšu<br>
> erbī bēltī kutâ lirīški<br>
> ekal kurnugi lihdu ina pānīki
>
> The gatekeeper went, and opened his gate,<br>
> "Enter, ladyship, and let Kutha rejoice,<br>
> Let the palace of the Netherworld rejoice in your presence."

Fun

> ištēn bāba ušēribši-ma umtaṣṣi ittabal agâ rabâ ša qaqqadīša<br>
> ammīni atû tatbal agâ rabâ ša qaqqadīya<br>
> erbī bēltī ša bēlet erṣetim kī’am parṣūša
>
> He brought her through the first gate: he removed and took the great crown from her head.<br>
> "Why, gatekeeper, did you take the great crown from my head?"<br>
> "Enter, ladyship.  These are the rites of the mistress of the Netherworld."

fact: we

> šanâ bāba ušēribši-ma umtaṣṣi ittabal inṣabāte ša uznīša<br>
> ammīni atû tatbal inṣabāte ša uznīya<br>
> erbī bēltī ša bēlet erṣetim kī’am parṣūša
>
> He brought her through the second gate: he removed and took the rings from her ears.<br>
> "Why, gatekeeper, did you take the rings from my ears?"<br>
> "Enter, ladyship.  These are the rites of the mistress of the Netherworld."

don't really know

> šalšu bāba ušēribši-ma umtaṣṣi ittabal erimmāti ša kišādīša<br>
> ammīni atû tatbal erimmāti ša kišādiya<br>
> erbī bēltī ša bēlet erṣetim kī’am parṣūša
>
> He brought her through the third gate: he removed and took the beads from her neck.<br>
> "Why, gatekeeper, did you take the beads from my neck?"<br>
> "Enter, ladyship.  These are the rites of the mistress of the Netherworld."

how the names of

> rebû bāba ušēribši-ma umtaṣṣi ittabal dudinnāte ša irtīša<br>
> ammīni atû tatbal dudinnāte ša irtīya<br>
> erbī bēltī ša bēlet erṣetim kī’am parṣūša
>
> He brought her through the fourth gate: he removed and took the broaches from her breast.<br>
> "Why, gatekeeper, did you take the broaches from my breast?"<br>
> "Enter, ladyship.  These are the rites of the mistress of the Netherworld."

the Akkadian numbers are constructed

> hamšu bāba ušēribši-ma umtaṣṣi ittabal šibbu aban alādi ša qablīša<br>
> ammīni atû tatbal šibbu aban alādi ša qablīya<br>
> erbī bēltī ša bēlet erṣetim kī’am parṣūša
>
> He brought her through the fifth gate: he removed and took the belt of birthstones from her waist.<br>
> "Why, gatekeeper, did you take the belt of birthstones from my waist?"<br>
> "Enter, ladyship.  These are the rites of the mistress of the Netherworld."

for sure!  But this isn't so

> šeššu bāba ušēribši-ma umtaṣṣi ittabal šemerī qātīša u šēpīša<br>
> ammīni atû tatbal šemerī qātīya u šēpīya<br>
> erbī bēltī ša bēlet erṣetim kī’am parṣūša
>
> He brought her through the sixth gate: he removed and took the rings of her hands and feet.<br>
> "Why, gatekeeper, did you take the rings of my hands and feet?"<br>
> "Enter, ladyship.  These are the rites of the mistress of the Netherworld."

<konata:point>
The "rings of her hands and feet" are, according to other translations,
bracelets and anklets.  Not finger and toe rings as I was imagining.
</konata>

> sebû bāba ušēribši-ma umtaṣṣi ittabal ṣubāt balti ša zumrīša<br>
> ammīni atû tatbal ṣubāt balti ša zumrīya<br>
> erbī bēltī ša bēlet erṣetim kī’am parṣūša
>
> He brought her through the seventh gate: he removed and took the formal garments from her body.<br>
> "Why, gatekeeper, did you take the formal garments from my body?"<br>
> "Enter, ladyship.  These are the rites of the mistress of the Netherworld."

bad because people mostly write with digits.

### Lines 64–75: The Sixty Diseases of the Netherworld

> ištu ullânum-ma ištar ana kurnugi ūridu<br>
> ereškigal īmurši-ma ina pānīša ir’ub<br>
> ištar ul immalik elênušša ušbi<br>
> ereškigal pâša īpuš-ma iqabbi<br>
> ana namtar sukkallīša amātum izzakkar<br>
> alik namtar [...] / šūṣâšši šūši murṣī ana rabīti ištar<br>
> muruṣ īni ana īnīša<br>
> muruṣ ahi ana ahīša<br>
> muruṣ šēpī ana šēpīša<br>
> muruṣ libbi ana libbīša<br>
> muruṣ qaqqadi ana qaqqadīša<br>
> ana šâša gabbīša-ma šūṣâšši murṣī

> From the very moment Ištar went down to the Netherworld,<br>
> Ereškigal saw her: her face trembled with anger.<br>
> Ištar, unaware, sat above,<br>
> Ereškigal made ready to speak,<br>
> To Namtar her chief minister she speaks a command:<br>
> "Go Namtar [...] send out 60 diseases against Ištar the Great:<br>
> Eye disease against her eyes;<br>
> Arm disease against her arms;<br>
> Foot disease against her feet;<br>
> Heart disease against her heart;<br>
> Head disease against her head."<br>
> Disease went out to all of Ištar. **[lit: to her totality]**

Here is the second great mistake!  I got that final line utterly wrong.  To
revisit it and translate it quite literally, word-by-word, it should be more
like:

> against her, all of her: you [masculine, singular] cause to go out to her
> [imperative] diseases!

So this is actually still part of Ereškigal's command to Namtar: "send out
diseases to all of Ištar!"

<konata:glasses>
"šūṣâšši" took me a while to figure out.  I could tell it was a form of the verb
"aṣûm" ("to go out") but I couldn't figure it out exactly until when I was going
through the poem again after finishing the translation.

There are essentially three forms of Babylonian, not counting regional dialects:

- Old Babylonian is the most lexically rigid: different verb- and noun-forms are
  pretty clearly distinguished, it's the easiest to learn.
- Middle Babylonian is somewhat more relaxed, a lot of word endings are omitted,
  and the case system has started to break down a bit, which can cause other
  spelling changes.
- Standard Babylonian is the most chaotic, case-endings of words are more or
  less included at random so you have to rely on context to determine grammar a
  lot more.  The vast bulk of the tablets we have today are written in Standard
  Babylonian.

"šūṣâšši" is specifically a Middle Babylonian spelling of the 2nd person
masculine singular Š-imperative of "aṣûm", meaning "you [masculine, singular]
cause to go out to her".  The Old Babylonian spelling, which is what I was
looking at originally, is "šūṣiaššim".

Check it out in [BVC](https://www.gilgamesh.ch/bvc/bvc.html?&opt_ventive=1&opt_ind_obj=1&ass_st_to_lt=1&ass_bb_to_mb=1&stem=%C5%A0&verbpattern=3&verbroot=1320&verbrootx=wa%E1%B9%A3%C3%BBm&opt_sel_ind_obj=1&ivar=1)!
</konata>

### Lines 76–80: The Death of Ištar (and sex)

> arki ištar bēltī ana kurnugi ūridu<br>
> ana būrti alpu ul išahhiṭ imēru atānu ul ušarra<br>
> ardatum ina sūqi ul ušarrâ eṭlu<br>
> ittīl eṭlu ina kummīšu<br>
> ittīl ardatum ina ahītīša

> Henceforth Great Ištar went down into the Netherworld.<br>
> Oxen stopped jumping on cows, and donkeys on jennys, **[lit: "do not jump"]**<br>
> Young women in the street stopped laying with young men. **[lit: "do not lay down"]**<br>
> Lay the young man in the innermost room of his house.<br>
> Lay the young woman on her side.

I think the last three lines here must have figurative meanings that I'm
missing.  Well, I'm assuming people in Babylon didn't just have sex on the
street.  One translation I've looked at gave the final two lines as "the young
man sleeps in his bedroom" / "the young woman sleeps by herself."

I suppose spooning was unknown in Babylon.

### Lines 81–90: The gods notice a distinct lack of earthly sex

> papsukkal sukkal ilī rabûti quddud appašu pānūšu arpū<br>
> karru labiš malê naškun<br>
> illik anhiš ina pān sîn abīšu ibakki<br>
> ina pān ea šarri illakā dimāšu<br>
> ištar ana erṣetim ūrid ul īlâ<br>
> ultu ullânum-ma ištar ana kurnugi ūridu<br>
> ana būrti alpu ul išahhiṭ imēru atānu ul ušarra<br>
> ardatum ina sūqi ul ušarra eṭlu<br>
> ittīl eṭlu ina kummīšu<br>
> ittīl ardatum ina ahītīša

> Papsukkal, vizier of the great gods, bent low, his nose and face clouded over,<br>
> Wearing mourning clothes, with unkempt hair, **[lit: he is/was supplied with unkempt hair]**<br>
> He went, tired, before Sîn, his father, weeping,<br>
> His tears went before Ea, King of the Gods:<br>
> "Ištar went down to the Netherworld and has not come up,<br>
> From the very moment Ištar went down to the Netherworld,<br>
> Oxen stopped jumping on cows, and donkeys on jennys,<br>
> Young women in the street stopped laying with young men.<br>
> Lay the young man in the innermost room of his house.<br>
> Lay the young woman on her side."

Another repetitive bit.  I do like how Papsukkal, vizier of the gods, has the
word "vizier" ("sukkal") in his name.  Nominative determinism is an old and
respected tradition.

### Lines 91–99: Ea decides Ereškigal just needs to get laid

> ea ina emqi libbīšu ibtani zikru<br>
> ibnī-ma aṣûšu-namir assinnu<br>
> alka aṣûšu-namir ina bāb kurnugi šukun pānīka<br>
> sebet bābū kurnugi lippetû ina pānīka<br>
> ereškigal līmurka-ma ina pānīka lihdu<br>
> ultu libbaša inuhhu kabtassa ippereddû<br>
> tummeši-ma nīš ilī rabûti<br>
> šuqi rēšīka ana halziqqi uzna šukun<br>
> ē bēltī halziqqu lidnūni mê ina libbi luštatti

> Ea, in the wisdom of his heart, created a man,<br>
> He created Aṣûšu-namir, a male cultic prostitute.<br>
> "Go, Aṣûšu-namir, show your face at the gate of the Netherworld,<br>
> May the seven gates of the Netherworld be opened before you, **[lit: before your face]**<br>
> May Ereškigal see you, and rejoice in your presence,<br>
> After her heart rests, her mood improves, **[lit: "her liver shines brightly"]**<br>
> Make her speak the oath of the great gods,<br>
> Raise your head, put your ear to the waterskin,<br>
> 'O my lady, may they give me the waterskin, may I drink its waters.'"

Another small mistake here, and this one is a little embarrassing: Ea tells
Aṣûšu-namir to "put your ear to the waterskin", which of course means to "turn
your attention / desire to the waterskin": we saw that in the second and third
lines of the poem!  But I missed that, whoops.

### Lines 100–108: Ereškigal gets mad

> ereškigal annīta ina šemîša<br>
> tamhaṣ pēmša taššuka ubānša<br>
> tēteršanni erištum ša lā erēši<br>
> alka aṣûšu-namir lūzzerka izra rabâ<br>
> aklī epinnēt āli lū akalka<br>
> habannāt āli lū maltītka<br>
> ṣillī dūri lū manzāzūka<br>
> askuppātu lū mušābūka<br>
> šakru u ṣamû limhaṣū lētka

> Ereškigal, in hearing this matter,<br>
> Beat her thigh, bit her finger,<br>
> "You have asked me a question that should not be asked! **[lit: "of no asking"]**<br>
> Go, Aṣûšu-namir, may I curse you with a great curse!<br>
> May the bread of the ploughs of the city be your bread!<br>
> May the habannatu-containers of the city be your cup! **[note: habannatu-containers may have been used for sewage]**<br>
> May the shade of the city wall be your position!<br>
> May the doorstep be your home!<br>
> May drink and thirst strike your cheek!"

One funny thing about reading the dictionary (wow, what a sentence) is that it
has quotations and commentary, and this one:

> aklī epinnēt āli lū akalka

Is right under the "epinnum" ("plough") entry with the comment "uncertain
meaning".

<konata:ponder>
I assume the whole waterskin thing is something to do with the practice of
offering water to the ghosts of the dead, see also when Ereškigal said "Do I
drink water with the Anunnaki?"
</konata>

### Lines 109–114: Ereškigal sends out Ištar

> ereškigal pâša īpuš-ma iqabbi<br>
> ana namtar sukkallīša amāta izzakkar<br>
> alik namtar mahaṣ ēkalla kitta<br>
> askuppāti za’’ina ša ayyarāti<br>
> anunnakī šūṣâ ina kussi hurāṣi šūšib<br>
> ištar mê balāṭi suluhši-ma leqâšši ina mahrīya

> Ereškigal made ready to speak,<br>
> To Namtar her chief minister she speaks commands:<br>
> "Go, Namtar, strike the true palace,<br>
> Decorate the threshold slab with white coral,<br>
> Bring out the Anunnaki, seat them in gold thrones,<br>
> Sprinkle Ištar with the water of life; take her outside." **[lit: take her to the front]**

And now we come to the third great mistake!  I translated "mahrīya" as "the
front", i.e. "the front [of the Netherworld]", or "outside".  But the "ya"
ending makes it "*my* front".  So that should actually be "bring her before me."

<konata:glasses>
Akkadian doesn't really distinguish "direction" in verbs.  The same word could
be translated as "take" or "bring" depending on the context.  There is an ending
that means "to me" and is often used to disambiguate, but it's not used all the
time.

In general, Akkadian verbs have a far wider range of potential meanings than
English ones.
</konata>

### Lines 115–125: The Ascent of Ištar from the Netherworld

> illik namtar imhaṣ ēkalla kitta<br>
> askuppāti uza’’inā ša ayyarāti<br>
> anunnakī ušēṣâ ina kussi hurāṣi ušēšib<br>
> ištar mê balāṭi isluhši-ma ilqâšši ana mahrīša<br>
> ištēn bāba ušēṣiši-ma utterši ṣubāt balti ša zumrīša<br>
> šanâ bāba ušēṣiši-ma utterši šemer qātīša u šēpīša<br>
> šalša bāba ušēṣiši-ma utterši šibbu aban alādi ša qablīša<br>
> rebû bāba ušēṣiši-ma utterši dudinnāte ša irtīša<br>
> hamšu bāba ušēṣiši-ma utterši erimmāti ša kišādīša<br>
> šeššu bāba ušēṣiši-ma utterši inṣabāte ša uznīša<br>
> sebû bāba ušēṣiši-ma utterši agû rabâ ša qaqqadīša

> Namtar went and struck the true palace,<br>
> He decorated the threshold slab with white coral,<br>
> He brought out the Anunnaki and seated them in gold thrones,<br>
> He sprinkled Ištar with the water of life; he took her outside:<br>
> He took her out through the first gate: he returned the formal garments of her body.<br>
> He took her out through the second gate: he returned the rings of her hands and feet.<br>
> He took her out through the third gate: he returned the belt of birthstones of her waist.<br>
> He took her out through the fourth gate: he returned the broaches of her breast.<br>
> He took her out through the fifth gate: he returned the beads of her neck.<br>
> He took her out through the sixth gate: he returned the rings of her ears.<br>
> He took her out through the seventh gate: he returned the great crown of her head.

It would have been rather nice if this counted down for the gates rather than
up, but at least it got Ištar's regalia in the correct (i.e. reversed) order.

That's it, that's the poem!

<figure>
  <img src="posts/fate-ishtar-smug.jpg" alt="Ištar (from 'Fate/Grand Order) looking smug">
  <figcaption>Ištar (from Fate/Grand Order) looking smug</figcaption>
</figure>

## What's next?

Next I'll put together an annotated line-by-line translation, like I did for
[Ea-Nāṣir][eanasir].  I don't think I'll do *word-by-word* annotations again, as
that was quite a lot of work and this poem is over twice as long, but I'll
definitely comment on the more interesting parts of each line.

I expect to have that done in the next couple of weeks.

After that, I guess it's back to studying.  Working with real texts helps a lot,
but reading about the grammar and working through exercises is important too.
As for the next translation, I think it would be nice to finally tackle some
cuneiform, maybe I'll pick a letter or a lengthy inscription, and work my way up
to cuneiform literature.

[Advent of Code]: https://adventofcode.com/
[Altvent of Code]: https://wlcx.cc/projects/altvent-of-code/
[source1]: https://www.soas.ac.uk/baplar/recordings/istars-descent-netherworld-lines-1-125-read-martin-west
[source2]: https://cdli.mpiwg-berlin.mpg.de/artifacts/497322
[cad]: https://isac.uchicago.edu/research/publications/chicago-assyrian-dictionary
[bvc]:https://www.gilgamesh.ch/bvc/bvc.html 
[eanasir]: akkadian/ea-nasir.html
