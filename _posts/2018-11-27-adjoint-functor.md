---
layout: post
title: Adjoint Functor
excerpt: Notes on the category theory concept.
category: ["Category Theory"]
modified: 2018-11-27
---

The concept "adjoint functors" pops out quite often when I'm browsing some interesting things. So this time I try to understand what exactly "adjoint functor" is by taking notes.

## Definition

_the definitions comes from [Wikipedia](https://en.wikipedia.org/wiki/Adjoint_functors#Definition_via_universal_morphisms)_

### Simpler Definition

An adjunction between categories $$ \mathcal{C} $$ and $$ \mathcal{D} $$ is a pair of functors (assumed to be covariant)

$$ F:{\mathcal {D}}\rightarrow {\mathcal {C}} $$ and $$ G:{\mathcal {C}}\rightarrow {\mathcal {D}} $$

And,

$$ \forall X \in \mathcal{C}, Y \in \mathcal{D}. \mathrm {hom} _{\mathcal {C}}(FY,X) \cong \mathrm {hom} _{\mathcal {D}}(Y,GX) $$

Then we say functor $$ F $$ is left adjoint to $$ G $$, $$ G $$ is right adjoint to $$ F $$.

### Via [Universal Morphisms](https://en.wikipedia.org/wiki/Universal_property)

A funtor $$ F: D \to C $$ is a **left adjoint functor** to functor $$ G : C \to D $$ if: 

$$ \forall X \in ob(C). \exists $$ _terminal morphism_ $$ (G(X), \epsilon_X) $$ from $$ F $$ to $$ X $$.

That is,

$$ \forall X \in ob(C). \exists \epsilon_X : F(G(X)) \to X. \forall Y \in ob(D), f : F(Y) \to X. \exists ! g : Y \to G(X). F(g) \circ \epsilon_X = f $$

![left_adjoint_diagram](https://wikimedia.org/api/rest_v1/media/math/render/svg/44af0381f1f2cc20703f9a55646dc8aa3b0dd14d)

Conversely,

a functor $$ G : C \to D $$ is a **right adjoint functor** to functor $$ F : D \to C $$ if:

$$ \forall Y \in ob(D). \exists $$ _initial morphism_ $$ (F(Y), \eta_Y) $$ from $$ Y $$ to $$ G $$

That is,

$$ \forall Y \in ob(D). \exists \eta_Y : Y \to G(F(Y)). \forall X \in ob(C), g : Y \to G(X). \exists ! f : F(Y) \to X. \eta_Y \circ G(f) = g $$

![right_adjoint_diagram](https://wikimedia.org/api/rest_v1/media/math/render/svg/0a3195dd654be31dc101171065cec60e6c1326ff)

## Examples

### An example for five-year-old

This example is from [an answer on MathOverflow](https://mathoverflow.net/a/51659). It leads with

> The example I would give a five-year old is the following.

Consider the categories of $$ \mathbb{Z} $$ and $$ \mathbb{R} $$ whose objects are integers and real numbers respectively, morphisms of $$ x \to y $$ exists whenever $$ x \le y $$.

For functor $$ inclusion : \mathbb{Z} \to \mathbb{R} $$, the left adjoint of it can be $$ ceiling : \mathbb{R} \to \mathbb{Z} $$, the right adjoint of it can be $$ floor : \mathbb{R} \to \mathbb{Z} $$

That is:

$$ \lceil x \rceil \le y \Leftrightarrow x \le y $$

and 

$$ x \le y \Leftrightarrow x \le \lfloor y \rfloor $$

Note: if we reverse the order of left and right adjoint (left is floor, right is ceiling), then the equation $$ \lfloor x \rfloor \le y \Leftrightarrow x \le y $$ doesn't always hold (e.g. x = 1.5, y = 1), so does the right adjoint one.

Unsurprisingly, the property of universal morphisms also express the similar idea.

For left adjoint (ceiling), it says: for every real number $$ x $$ whose ceiling is less than or equal to an integer $$ y $$, itself is less than or equal to $$ y $$('s real number form)

For right adjoint (floor), it says: for every real number $$ x $$ whose floor is greater than an integer $$ y $$, itself is greater than $$ y $$('s real number form).
