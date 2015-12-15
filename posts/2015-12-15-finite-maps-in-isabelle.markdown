---
title: Finite Maps in Isabelle
description: A whirlwind tour of theorem proving in the Isabelle/HOL
  proof assistant.
---

[Isabelle][] (or Isabelle/HOL) is a proof assistant supporting some
degree of automated reasoning, and can be programmed using a dialect
of ML. This makes it perfect for proving properties of functional
programs (there's even a converter from Haskell to Isabelle), and I've
been learning it for the past month or so with the intention of
proving correct bits of [Déjà Fu][].

Recently I "finished" a theory of finite maps in Isabelle, which I'm
going to use to construct a theory of [finite tries][trie]. Then, if
you squint hard enough at the `BPOR` type defined in
`Test.DejaFu.SCT.Internal`, it's a finite trie! Because it's a fairly
awkward data structure, I want to get all this foundational work done
well, to ease reasoning about the higher-level thing.

[Isabelle]: https://isabelle.in.tum.de/
[Déjà Fu]: https://github.com/barrucadu/dejafu
[trie]: https://en.wikipedia.org/wiki/Trie

### Theories

An Isabelle source file is called a *theory*, and is a collection of
types, definitions, functions, lemmas, theorems, and so on. A theory
starts with a header which specifies its name (which must match the
source file) and what theories this builds upon:

```isabelle
theory FMap
imports BNF_Cardinal_Arithmetic BNF_Def Map FSet
begin
```

Theories can also contain commentary, which can be compiled to
LaTeX. Various section types as well as the ability to embed arbitrary
TeX exist. With Isabelle, the culture seems to be that the code *is*
the documentation. It certainly makes getting a paper out of your work
easy.

```isabelle
section {* Finite Maps *}
```

### Types

There are two ways to define new types in Isabelle: through the
*datatype* command, which is used to create regular sum and product
types; and through the *typedef* command, which is used to impose
constraints on already existing types.

I wanted to reuse the original map type to make proving things about
this as easy as possible, so I went for a *typedef* over that, and
just imposed the finiteness constraint:

```isabelle
typedef ('k, 'v) fmap = "{M :: ('k ⇀ 'v). finite (dom M)}" morphisms fmap Abs_fmap
proof -
  have "finite (dom empty)"
    by simp
  thus ?thesis
    by blast
qed
```

All types in Isabelle are inhabited, so a proof obligation is
generated here of the form `∃x. x ∈ {M. finite (dom M)}`. I have
proven this by providing a witness: `empty` is the empty map which,
naturally, has a finite domain. `simp` and `blast` are different
proovers: `simp` applies a collection of rewrite rules, called
simplification rules; `blast` is one of the few different solvers for
first-order logic.

This type definition also gives us morphisms to convert a regular map
to our new finite map type and back, provided that the finiteness
requirement holds.

Rather than write our types out as `('k, 'v) fmap` all the time, new
syntax can be defined to make the theories look prettier:

```isabelle
type_notation "fmap" (infixr "|~=>|" 0)
type_notation (xsymbols) "fmap" (infixr "↪"  0)
```

I spent quite some time looking for the unicode arrow which felt right
to me but, alas, didn't find the *perfect* arrow.

### Lifting

Isabelle lets us reuse definitions and functions using the base type
with the abstract type, as long as we can prove that the invariant is
satisfied. This is called *lifting*, and we need to explicitly turn it
on for a type:

```isabelle
setup_lifting type_definition_fmap

lift_definition dom :: "('k ↪ 'v) ⇒ 'k set"
is "Map.dom".

lift_definition ran :: "('k ↪ 'v) ⇒ 'v set"
is "Map.ran".
```

In general, lifted definitions come with a proof obligation that the
invariants of the abstract type get preserved, but here this is
sufficiently trivial that `lift_definition` completely handles it for
us.

```isabelle
lift_definition elems :: "('k ↪ 'v) ⇒ ('k × 'v) set"
is "λ m. {(a, b). a ∈ Map.dom m ∧ b = the (m a)}".
```

Maps are defined as a function from `'k` to `'v option`, which if
you've used Haskell is just `Maybe`. The function `the` extracts a
value when there is one.

The reason `elems` is defined like is to make the later proofs of
`dom_elems` and `ran_elems` simpler. However, the more obvious
formulation is also useful to have:

```isabelle
lemma elems_def_alt: "elems m = {(k, v). fmap m k = Some v}"
by (smt Collect_cong case_prod_unfold domI domIff elems.rep_eq option.collapse option.sel)
```

The `smt` prover is used here which, given a collection of lemmas and
definitions, attempts to prove the goal by finding a satisfying
assignment of the free variables.

Naturally, the domain, range, and elements are all related:

```isabelle
lemma dom_elems: "dom m = fst ` elems m"
by (transfer, force)
```

`transfer` is a prover which attempts to use the relation between the
concrete and the abstract types to simplify the goal. `force` is
another of the first-order logic provers. It doesn't always work, but
when it does, `transfer` vastly simplifies proofs involving lifted
definitions.

The proof for `ran_elems` is a little less simple:

```isabelle
lemma ran_elems: "ran m = snd ` elems m"
  apply (transfer, simp add: Map.ran_def)
by force
```

There's an extra simplification step here, because the definition of
`elems` isn't as obviously related to `ran` as it is to `dom`. `apply`
applies the given provers, in this case, `transfer` and `simp`. Proofs
with lots of `apply` lines are a bit difficult to read, so structured
proofs written in the Isar language tend to be preferred for difficult
goals.

We also know that the domain, range, and elements are finite sets, by
the definition of the type; so it's no surprise that we can prove
these properties. Here an Isar proof has been used to show the
reasoning steps more clearly:

```isabelle
lemma elems_finite: "finite (elems m)"
apply transfer
unfolding Map.dom_def
apply simp

(* At this stage, the proof obligation is:

   ⋀m. finite {a. ∃y. m a = Some y} ⟹
       finite {(a, b). (∃y. m a = Some y) ∧ b = the (m a)}
 *)

proof -
  fix m
  assume "finite {a. ∃y. m a = Some y}"
  hence "finite ((λ k. (k, the (m k))) ` {a. ∃y. m a = Some y})"
    by simp
  hence "finite {(a, b). (∃ y. m a = Some y) ∧ b = the (m a)}"
    apply (simp add: image_def)
    by (smt Collect_cong case_prod_unfold fst_conv prod.collapse snd_conv)
  thus "finite {(a, b). (∃y. m a = Some y) ∧ b = the (m a)}"
    by auto
qed

lemma dom_finite: "finite (dom m)"
by (simp add: elems_finite dom_elems)

lemma ran_finite: "finite (ran m)"
by (simp add: elems_finite ran_elems)
```

Isabelle has two levels of logic, on term-level bools and on the
meta-logic. `⋀` is the meta-logic equivalent of `∀`; `⟹` the
equivalent of `⟶`. In general the automated reasoning tools can
decompose things expressed in the meta-logic more easily than they can
things expressed in terms of bools, and so it should be preferred
wherever it can be used.

Note that Isar and apply-style proofs can be mixed. This is good
because sometimes a short chain of `apply` invocations will do the
job, where writing out each step explicitly would just be needlessly
verbose.

We can also prove some facts about the cardinality of these sets, due
to two facts: keys are unique (as it's just a function), but values
are not necessarily. Therefore there are as many elements as there are
keys, and at most as many values as there are keys.

It's convenient to approach this by first proving an auxiliary lemma
about the underlying map type:

```isabelle
lemma map_elems_card: "card {(a, b). m a = Some b} = card (Map.dom m)"
proof -
  have "Map.dom m = fst ` {(a, b). m a = Some b}"
    by force
  also have "card … = card {(a, b). m a = Some b}"
    by (subst card_image, auto simp: inj_on_def)
  finally show ?thesis by simp
qed
```

Here `…` expands to the right-hand side of the prior proof line. More
specifically, it pattern-matches the prior line against `?P ?x ?y` and
returns `?y`. It's convenient to avoid repeating things.

This makes the corresponding `fmap` lemmas very simple:

```isabelle
lemma dom_elems_card: "card (dom m) = card (elems m)"
by (simp add: elems_def_alt FMap.dom_def map_elems_card)

lemma dom_ran_card: "card (dom m) ≥ card (ran m)"
by (simp add: ran_elems dom_elems_card elems_finite card_image_le)

lemma ran_dom_card: "card (ran m) ≤ card (dom m)"
by (simp add: dom_ran_card)

lemma ran_elems_card: "card (ran m) ≤ card (elems m)"
by (metis dom_elems_card dom_ran_card)
```

### The empty map

As the empty map was used as a witness earlier in the definition of
`fmap`, it's no surprise that we might want to lift it to `fmap`
directly:

```isabelle
lift_definition empty :: "('k ↪ 'v)"
is "Map.empty"
by simp
```

Here the proof obligation is not so simple it can be solved by
`lift_definition` itself, but one call to `simp` is all that is needed
to complete it.

```isabelle
lemma elems_empty: "elems x = {} ⟷ x = empty"
by (transfer, auto)

lemma dom_empty: "dom x = {} ⟷ x = empty"
by (auto simp add: dom_elems elems_empty)

lemma ran_empty: "ran x = {} ⟷ x = empty"
by (auto simp add: ran_elems elems_empty)
```

### Functorial mapping

It should come as no surprise to anyone familiar with
Hindley-Milner-style parametric polymorphism that `fmap` is a functor
when the key type is fixed! We can define the mapping function quite
simply:

```isabelle
lift_definition fmap_map :: "('v ⇒ 'w) ⇒ ('k ↪ 'v) ⇒ ('k ↪ 'w)"
is "λ f m x. Option.bind (m x) (Some ∘ f)"
by (smt Collect_cong Map.dom_def bind.simps comp_def domD mem_Collect_eq option.distinct)
```

Then we have the functor laws to prove for this:

```isabelle
lemma fmap_map_id: "fmap_map id m = m"
by (auto simp add: fmap_map_def map_fun_def fmap_inverse)

lemma fmap_map_comp: "fmap_map (g ∘ f) m = fmap_map g (fmap_map f m)"
  apply transfer
  apply auto
by (simp add: comp_def)
```

To make other things easier later on, we'll also prove some additional
lemmas which, strictly speaking, we don't need as we can derive from
what we have already:

```isabelle
lemma fmap_map_dom: "dom m = dom (fmap_map f m)"
  apply transfer
  apply auto
by (meson bind_eq_Some_conv)

lemma fmap_map_ran: "ran (fmap_map f m) = f ` ran m"
  apply transfer
  apply auto
  apply (smt CollectD Map.ran_def bind_eq_Some_conv comp_apply image_iff option.sel ranI)
by (smt Map.ran_def bind_lunit comp_eq_dest_lhs mem_Collect_eq)
```

### Relations

We can lift a relation over pairs of values to a relation over pairs
of maps, where elements with the same key are compared:

```isabelle
lift_definition fmap_rel :: "('v ⇒ 'w ⇒ bool) ⇒ ('k ↪ 'v) ⇒ ('k ↪ 'w) ⇒ bool"
is "λ R m1 m2. (dom m1 = dom m2 ∧
  (∀ x ∈ dom m1. R (the (fmap m1 x)) (the (fmap m2 x))))".
```

We can also define a notion of zipping maps together, where elements
with the same key are tupled up. This clearly relates to the
`fmap_rel` definition just given:

```isabelle
axiomatization fmap_zip :: "('k ↪ 'v) ⇒ ('k ↪ 'w) ⇒ ('k ↪ 'v × 'w)"
where
  fmap_zip_dom: "dom (fmap_zip m1 m2) = {k. fmap m1 k ≠ None ∧ fmap m2 k ≠ None}" and
  fmap_zip_ran: "ran (fmap_zip m1 m2) = {(a,b). ∃ k. fmap m1 k = Some a ∧ fmap m2 k = Some b}" and
  fmap_zip_fst_ran: "fst ` ran (fmap_zip m1 m2) ⊆ ran m1" and
  fmap_zip_fst: "dom m1 = dom m2 ⟹ m1 = fmap_map fst (fmap_zip m1 m2)" and
  fmap_zip_snd_ran: "snd ` ran (fmap_zip m1 m2) ⊆ ran m2" and
  fmap_zip_snd: "dom m1 = dom m2 ⟹ m2 = fmap_map snd (fmap_zip m1 m2)" and
  fmap_zip_rel: "fmap_rel R m1 m2 ⟷ ran (fmap_zip m1 m2) ⊆ {(x, y). R x y}"
```

I have not provided a definition for `fmap_zip` directly, instead I
have merely given a collection of named axioms it satisfies. This is
yet another way of defining things in Isabelle, and is useful for
reasoning about abstract concepts for which there perhaps isn't a
single sensible definition. I went for this approach because I
couldn't come up with a definition for `fmap_zip` which was nice
enough to let me prove all the lemmas I wanted, which is not a very
good motivation.

### Bounded natural functors

Isabelle supports directly and indirectly recursive datatypes. By
*directly* recursive, I mean something like this:

```isabelle
datatype 'a binary_tree = Leaf 'a | Branch "'a binary_tree" "'a binary_tree"
```

Here the recursion in `binary_tree` happens on a `binary_tree`
value. Whereas *indirect* recursion would be something like this:

```isabelle
datatype 'a rose_tree = Leaf 'a | Branch "'a rose_tree list"
```

Where there is an intermediary type in the way, `list` in this
case. Not just any type can be used there, it must be a type which has
been registered as a *bounded natural functor*.

A bounded natural functor (or BNF) is a functor which has some
additional natural transformations associated with it to do with
relations, and which preserves finiteness. Fortunately we've almost
done everything we need in order to prove that `fmap` is a BNF!

Just before we do, it's convenient to relate the two notions of set
cardinality in Isabelle. There is cardinality of sets, and there is a
more generic notion of cardinality relating to types, which BNFs use:

```isabelle
lemma card_leq_ordLeq:
  assumes "finite x"
  assumes "finite y"
  assumes "card x ≤ card y"
  shows "ordLeq3 (card_of x) (card_of y)"
using assms
by (meson card_le_inj card_of_ordLeq)
```

The `assumes`/`shows` notation is a shorthand for `⟦assm1; …; assmnN⟧
⟹ shows`, which itself is a shorthand for `assm1 ⟹ … ⟹ assmN ⟹ shows`.

Registering a type as a BNF consists of providing some definitions,
and then proving that they satisfy a bunch of properties:

```isabelle
bnf "'k ↪ 'v"
  map: fmap_map
  sets: ran
  bd: "BNF_Cardinal_Arithmetic.csum natLeq (card_of (UNIV :: 'k set))"
  wits: empty
  rel: fmap_rel
```

This gives rise to 10 proof obligations. Isar proofs with multiple
obligations proceed by having multiple `show` lines, one for each
obligation. The `next` keyword can be used to split up the proof:

```isabelle
proof -
  show "fmap_map id = id"
    by (auto simp add: fmap_map_id)
next
  show "⋀ f g. fmap_map (g ∘ f) = fmap_map g ∘ fmap_map f"
    by (auto simp add: fmap_map_comp)
next
  show "⋀ x f g. (⋀z. z ∈ ran x ⟹ f z = g z) ⟹ fmap_map f x = fmap_map g x"
    apply transfer
    by (metis bind.simps comp_apply not_None_eq ranI)
next
  show "⋀f. ran ∘ fmap_map f = op ` f ∘ ran"
    by (auto simp add: fmap_map_ran)
```

Now we have some facts about cardinality that I don't *really*
understand. I *think* that it's first establishing that the
cardinality property given is a valid cardinality, and that the
definition provided for the `sets` returns a set within this
cardinality bound:

```isabelle
next
  show "card_order (BNF_Cardinal_Arithmetic.csum natLeq (card_of UNIV))"
    apply (rule card_order_csum)
    apply (rule natLeq_card_order)
    by (rule card_of_card_order_on)
next
  show "BNF_Cardinal_Arithmetic.cinfinite (BNF_Cardinal_Arithmetic.csum natLeq (card_of UNIV))"
    apply (rule cinfinite_csum)
    apply (rule disjI1)
    by (rule natLeq_cinfinite)
next
  fix m :: "('k, 'v) fmap"
  have "ordLeq3 (card_of (ran m)) (card_of (dom m))"
    by (simp add: ran_dom_card ran_finite dom_finite card_leq_ordLeq)
  also have "ordLeq3 (card_of (dom m)) (card_of (UNIV::'k set))" (is "ordLeq3 _ ?U")
    by (simp add: card_of_mono1)
  also have "ordLeq3 ?U (BNF_Cardinal_Arithmetic.csum natLeq ?U)"
    by (rule ordLeq_csum2) (rule card_of_Card_order)
  finally show "ordLeq3 (card_of (ran m)) (BNF_Cardinal_Arithmetic.csum natLeq ?U)"
    using ordLeq_transitive
    by blast
```

As I said, I don't really understand it. It's entirely possible I have
picked a wildly inappropriate cardinality bound, which is *way* bigger
than it needs to be. Oh well.

Next we have some facts about `OO`, relational composition:

```isabelle
next
  show "⋀R S. fmap_rel R OO fmap_rel S ≤ fmap_rel (R OO S)"
    by (smt fmap_rel.transfer pick_middlep predicate2I relcompp.relcompI)
next
  show "⋀R. fmap_rel R = (BNF_Def.Grp {x. ran x ⊆ {(x, y). R x y}} (fmap_map fst))¯¯ OO BNF_Def.Grp {x. ran x ⊆ {(x, y). R x y}} (fmap_map snd)"
    unfolding Grp_def fun_eq_iff relcompp.simps conversep.simps
    apply auto
    proof -
      show "⋀R x xa.
       fmap_rel R x xa ⟹
       ∃b. x = fmap_map fst b ∧
           ran b ⊆ {(x, y). R x y} ∧
           xa = fmap_map snd b ∧ ran b ⊆ {(x, y). R x y}"
        by (meson fmap_rel.abs_eq fmap_zip_fst fmap_zip_rel fmap_zip_snd)
    next
      show "⋀R b. ran b ⊆ {(x, y). R x y} ⟹
           fmap_rel R (fmap_map fst b) (fmap_map snd b)"
        unfolding fmap_rel_def
        apply auto
        apply (metis fmap_map_dom)
        apply (metis fmap_map_dom)
        proof -
        
(* At this stage the proof obligation is:

   ⋀R b x. ran b ⊆ {(x, y). R x y} ⟹
           x ∈ dom (fmap_map fst b) ⟹
           R (the (fmap (fmap_map fst b) x)) (the (fmap (fmap_map snd b) x))
 *)
 
          fix R m x
          assume "ran m ⊆ {(x, y). R x y}"
          assume "x ∈ dom (fmap_map fst m)"
          obtain a b where "fmap m x = Some (a, b)"
            by (metis `x ∈ FMap.dom (fmap_map fst m)` dom.rep_eq domD fmap_map_dom prod.swap_def swap_swap)
          have thea: "a = the (fmap (fmap_map fst m) x)"
            by (simp add: `fmap m x = Some (a, b)` fmap_map.rep_eq)
          have theb: "b = the (fmap (fmap_map snd m) x)"
            by (simp add: `fmap m x = Some (a, b)` fmap_map.rep_eq)
          have "R a b"
            using `FMap.ran m ⊆ {(x, y). R x y}` `fmap m x = Some (a, b)` ran.rep_eq ranI
            by fastforce
          thus "R (the (fmap (fmap_map fst m) x)) (the (fmap (fmap_map snd m) x))"
            by (simp add: thea theb)
        qed
    qed
```

The last obligation is very simple, the emptiness witness provided
contains no values:

```isabelle
next
  show "⋀x. x ∈ ran empty ⟹ False"
    by (simp add: empty.abs_eq empty.rsp ran.abs_eq)
qed
```

Finally, the `begin` at the start of the theory is closed:

```isabelle
end
```

Overall, I quite like Isabelle.

It's taken me a little while to get into the swing of things, but I
think I've got the hang of it now. There are still many things to
learn, but progress is being made. I'm slowly building up a library of
useful types and lemmas in my [isabelle-library][] repository, which I
hope will be useful for more than just proofs for Déjà Fu.

If you want to give Isabelle a try for yourself, you can download the
latest release from the [official website][isabelle], which also has
[documentation][]. There's also a [mailing list][], IRC channel
(#isabelle on freenode, a little slow but has a few knowledgeable
people), and the [Archive of Formal Proofs][afp], a collection of
theorems in Isabelle, some of which are explained quite well.

[isabelle-library]: https://github.com/barrucadu/isabelle-library
[isabelle]: https://isabelle.in.tum.de/
[documentation]: https://isabelle.in.tum.de/documentation.html
[mailing list]: https://lists.cam.ac.uk/pipermail/cl-isabelle-users/index.html
[afp]: http://afp.sourceforge.net/
