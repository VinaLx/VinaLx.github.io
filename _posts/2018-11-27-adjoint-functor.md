---
layout: post
title: Adjoint Functor
excerpt: Notes on the category theory concept.
category: ["Category Theory"]
modified: 2018-11-27
---

The concept "adjoint functors" pops out quite often when I'm browsing some interesting things. So this time I try to understand what exactly "adjoint functor" is by taking notes.

## Definition

_the definition comes from [Wikipedia](https://en.wikipedia.org/wiki/Adjoint_functors#Definition_via_universal_morphisms)_

A funtor $$ F: D \to C $$ is a left adjoint functor to functor $$ G : C \to D $$ if: 

$$ \forall X \in ob(C). \exists $$ [_terminal morphism_](https://en.wikipedia.org/wiki/Universal_property) $$ (G(X), \epsilon_X) $$ from $$ F $$ to $$ X $$.

That is,

$$ \forall X \in ob(C). \exists \epsilon_X \in hom_C(F(G(X)), X). \forall Y \in ob(D), f \in hom_C(F(Y), X). \exists ! g \in hom_D(Y, G(X)). F(g) \circ \epsilon_X = f $$

![left_adjoint_diagram](https://wikimedia.org/api/rest_v1/media/math/render/svg/44af0381f1f2cc20703f9a55646dc8aa3b0dd14d)

Conversely,

a functor $$ G : C \to D $$ is a right adjoint functor to functor $$ F : D \to C $$ if:

$$ \forall Y \in ob(D). \exists $$ [_initial morphism_](https://en.wikipedia.org/wiki/Universal_property) $$ (F(Y), \eta_Y) $$ from $$ Y $$ to $$ G $$

That is,

$$ \forall Y \in ob(D). \exists \eta_Y \in hom_D(Y, G(F(Y))). \forall X \in ob(C), g \in hom_D(Y, G(X)). \exists ! f \in hom_C(F(Y), X). \eta_y \circ G(f) = g $$

![right_adjoint_diagram](https://wikimedia.org/api/rest_v1/media/math/render/svg/0a3195dd654be31dc101171065cec60e6c1326ff)