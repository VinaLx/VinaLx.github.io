---
layout: post
title: "A Random Coq Coding Standard"
excerpt: "An unprofessional coq style guide for reference or collaboration"
category: ["recursion", "induction", "type theory"]
modified: 2021-10-16
---

## Tactics and Proofs

### General Tactic Styles

```
TODO
```

### Automation

#### Tactic Meta-language (Ltac / Ltac2)

```
TODO
```

#### Hint Database

- Use `Hint Immediate` (instead of `Hint Resolve`) for any lemma that potentially loops.
- Always use [`local` or `export`](https://coq.inria.fr/refman/language/core/modules.html#controlling-the-scope-of-commands-with-locality-attributes) [attributes](https://coq.inria.fr/refman/language/core/basic.html#grammar-token-attributes) to annotate a hint.
- Always specify hint database after a hint definition.

``` coq
Lemma eq_sym : forall {A : Type} {a b : A}, a = b -> b = a.
Proof. auto. Qed.

#[local]
Hint Immediate eq_sym : core.
```

## Naming

### Identifiers

- Following identifier categories follow `snake_case`
  + inductive types
  + data constructors
  + functions
  + theorems / lemmas

- Following identifier categories follow `camelCase`
  + local term-level parameters of functions
  + term-level variables in proof contexts for data

- Following identifier categories follow `CapitalCamelCase`
  + local type-level parameters of functions
  + type-level variables in proof contexts

``` coq
Definition leibniz_eq {A : Type} (a b : A) :=
  forall P : A -> Prop, P a -> P b.
```

### Proof Objects (Hypotheses) in Proof Contexts

```
TODO
```

### Bullets

- Use `-`, `+`, `*`, `--`, `++`, `**` in order from shallower level to deeper,
  although double-character bullets should generally be avoided.

## Code Formatting

### Spacing

- One space on each side of a binary operator in general, at both type-level (`->`, `<=`, `/\`)
  and term level (`+`, `*`).
- One space on each side of colon (`:`) (generally in type annotations).
- One space to the right of comma (`,`), no space to the left
  (generally after `forall`s, notations of cons operation of context, notation of pairs).

### Indent

- 2-space per indentation level in general.
- Full stops (`.`) of top-level commands should follow a linebreak when the said
  statement spans multiple lines (`Definition`, `Fixpoint`, `Inductive`, etc.),
  at the same indent level of their parent statements.

- `|`s should be at the same indent level as their parent statements /
  expressions (`match`, `Inductive` etc.)

``` coq
Definition negb (b : bool) : bool :=
  match b with
  | true => false
  | false => true
  end
.
```

- Indent levels of expressions after `=>`s should be one-level deeper than
  their parent statement (`match`, `fun` etc.)

``` coq
Definition negb (b : bool) : bool :=
  match b with
  | true =>
    false
  | false =>
    true
  end
.
```

- If function signatures span _more than_ 2 lines, arrows (`->` or `→`) and
  commas (`,`) should be the first syntactic token instead of the last in a line,
  with one-level deeper than their parent statement.
  + Put one more space after comma of the first `forall`
    expression if it's followed by `->` in the next line to align the parameter
    (there's no such need if using unicode right arrow `→`).

``` coq
(* signature only spans two lines *)
Definition foo : forall b : bool,
  bool := fun b => b.

(* signature spans more than two lines *)
Inductive bar : nat -> nat -> Type :=
| mk_bar : forall m n
  ,  m < n
  -> bar m n
.

Import Unicode.Utf8.

Inductive bar : nat → nat → Type :=
| mk_bar : ∀ m n
  , m < n
  → bar m n
.
```