---
layout: post
title: "Well-founded Recursion"
excerpt: "Generalizing structural recursion for languages with termination checker, part 1"
category: ["recursion", "induction", "type theory"]
modified: 2021-10-01
---

## Introduction

As we (may) know recursion is one of the most important elements in functional
programming, and one of the charm of recursion, usually paired with
algebraic data types is that the computation can be guaranteed to terminate
by the compiler, if implemented in a specific way, a.k.a. the
"[Structural Recursion](https://en.wikipedia.org/wiki/Structural_induction)".

For example:

```haskell
data Tree = Leaf | Branch Tree Int Tree

treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Branch l x r) = treeSum l + x + treeSum r
```

`treeSum` could be guaraneteed that it terminates regardless of
the input (although haskell doesn't do that by default), because the argument of
the recursive call `treeSum l` and `treeSum r` is _strictly smaller_ than the
parameter, in the sense that `l` and `r` are the "sub-structure" of the input
`Branch l x r`.

The structural recursion is even more important in theorem provers like Agda
and Coq, and the structural condition is strictly enforced by those compilers,
because otherwise we would be able to prove anything with an infinite loop.


```coq
(* Of course this would be rejected by Coq *)
Definition anything_is_true (A : Prop) : A := anything_is_true A.
```

However, the structural restriction is sometimes a bit too strong, even though
sometimes it's "easy" to tell that the structure of the arguments for the
recursive call should be decreasing, consider this famous sorting algorithm:

```haskell
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x : xs) = quickSort l ++ [x] ++ quickSort r
  where
    l = filter (<= x) xs
    r = filter (>  x) xs
```

Of course haskell doesn't argue with us about this recursive definition, but it
should be clear that this is **not** a structural recursion, as `l` and `r` here are
not necessarily structurally smaller than the argument `x : xs`. And language
like "Coq" would mercilessly reject a similar definition. Although coq is being
reasonable rejecting this definition, how do we convince the compiler that this
is indeed a safe recursive call, given the `filter` is implemented properly.
Years ago I implemented a similar thing in agda, and what I did is use a compiler
pragma to bypass the structural termination check, which surely is not a very
reliable way if we are going to implement something more complicated than this.

In these two (or three) articles, I'm going to share some of the interesting
techniques I learned which enable us to implement less restricted recursions,
without violating the structural recursion restriction.

For the sake of visual clarity I would use Idris (instead of Coq) as the sample
langauge which has a similar syntax to Haskell.

## Well-founded Recursion/Induction

### Well-founded Relation and the Finitely Descending Chain

Our first technique, probably the most practical one we are going to explore
, makes use of this set theory concept "[Well-founded Relation](https://en.wikipedia.org/wiki/Well-founded_relation)",
frankly the mathematical definition looks quite scary for those unfamiliar with the notations.
But the idea is, given a set $$ S $$ with a binary relation $$ R $$, and
with an arbitrary element $$ {a~|~a \in S} $$, we can find a chain with relation $$ R $$:
$$ R~a_0~a $$, $$ R~a_1~a_0 $$ , $$ R~a_2~a_1 $$ and it goes on.
If for any element $$ a \in S $$, this chain starts from $$ a $$ **stops** at some point,
i.e. $$ R~a_{n+1}~a_n $$ , but there does **NOT** exist $$ \{m~|~m \in S \land R~m~a_{n+1} \} $$,
then we say the relation is well-founded. For example, the "less-than"
($$ < $$) relation on natural number set is well-founded, because for any
arbitrarily chosen number $$ n $$ and an arbitrarily chosen "less-than chain",
the chain always stops at 0, because there isn't any natural number that is
less than 0.

Why is this useful? Well, not immediately, but it gives us a tangible approach
to convince someone who doesn't trust intuition (the compilers) that some recursive
call terminates: if we are given a natural
number `x` as argument for some recursive function `f`, then as long as we guarantee
the argument of all the recursive calls are _less than_ `x`, we guarantee the
recursion terminates, because "less-than" is a well-founded relation, and as
long as we follows the "chain" when making recursive calls, the chain has to
terminate at some point to respect the mathematical truth.

And we can conceptualize the "chain" with something like this:

```haskell
data ChainOf : (a -> a -> Type) -> a -> Type where
  Stop : (x : a) -> ((y : a) -> Not (DPair a (\y => r y x))) -> ChainOf r x
  GoOn : (x : a) -> (y : a) -> r y x -> ChainOf r y -> ChainOf r x

f : ChainOf r a -> ()
f (Stop x p) = ()                    -- do something with base case
f (GoOn x y p subChain) = f subChain -- do something between x and y
```

And we can safely define a recursive function with respect to the structure of
the chain constrained by the relation `r`, breaking free from having to recurse
on the structure of the exact value (`y` doesn't have to be a sub-structure of `x`,
yet the recursive call shift the argument from something about `x` to something about `y`).

However this definition of `ChainOf` isn't that practically useful, since we have
no control over the value of the sub-case of recursion,
we are given `y` and a proof that `r y x` holds, but when defining recursive function like `quickSort`,
what we want is a specific value of argument for sub-cases
(from `x : xs` to `filter (<=) xs`, for example), an arbitrary pre-selected choice wouldn't do.

### `Acc` and its Recursion Operator

So instead of recursing on a specific chain, we need a collection of chains from
which we can select the ones that suits our need. In most of the cases
the potential choices could be infinite, so we could do something like this:

```haskell
-- Acc is short for "Accessible"
data Acc : (a -> a -> Type) -> a -> Type where
  MkAcc : (x : a) -> ((y : a) -> r y x -> Acc r y) -> Acc r x
```

Here `Acc r x` reads: `x` is _accessible_ (from the end of the chain) using relation `r`.
The function `(y : a) -> r y x -> Acc r y` gives us a way to select the next step of recursion,
but of course we cannot just select an arbitrary value in the set (of type `a`), we need
to provide a proof that the `y` we choose indeed satisfies `r y x`.
So this simple, eh, probably not, this minimal definition captures all the
possible chains from the end of the chain to `Acc r x`.

But wait, how this single constructor expresses where the chain stops? The chain
stops when there isn't any `y : a` such that `r y x`, so the selection pool for
"next steps" is practically empty as we wouldn't be able to access any next
step `Acc r y` if we cannot prove any `r y x`.

And for those of you who are familiar with inductive/recursive principles, the
recursive principle for this type, or the fixpoint operator we are looking for is:

```haskell
accRec : {P : a -> Type}
      -> ((x : a) -> ((y : a) -> r y x -> P y) -> P x)
      -> {x : a} -> Acc r x -> P x
accRec rec (MkAcc x f) = rec x $ \y, ryx => accRec rec (f y ryx)
```

From the perspective of the definition of this operator, ignoring the the
longest parameter type, the type of the function says:
if `x` is _accessible_ with a certain `r`, when you give me _something_ (the parameter with the longest type),
I can help you run the recursive procedure and give you the final result (`P x`).
And the longest parameter type says: for all the possible `x`,
if you can show me how to compute the result for all `y` which is the next step
of the chain, I can show give you the result for `x`.

Then the definition is rather short since we only have one case to deal with,
the key is to show the recursive procedure (`rec`) that, starting from an
arbitrary sub-case (`y : a` with `r y x`), how to compute the result about `x` (`P x`),
which is indeed possible because the accessibility condition (`Acc r x`) helps
us prove that any sub-case `y` is accessible given `r y x`.
Then the recursive call of `accRec` handles the rest.

Before delving into the important details of this recursive definition, let's quickly
formulate the "well-founded" condition, and show you how they are used:

```haskell
wellFounded : (r : a -> a -> Type) -> Type
wellFounded {a} r = (x : a) -> Acc r x

-- simply delegates to accRec
-- as `wellFounded` says any value of `a` is accessible
wfRec : {P : a -> Type}
     -> ((x : a) -> ((y : a) -> r y x -> P y) -> P x)
     -> wellFounded r -> (x : a) -> P x
wfRec rec wf x = accRec rec (wf x)
```

### Well-founded Recursion and Revisiting Quicksort

Now let's go back to the quicksort and think about what are we doing with the list.
The well-founded relation we can find here, is that the argument lists for the
recursive call is guaranteed to be shorter than the the original argument.
And we know that the length of finite lists can only decrease finitely,
hence a comparison on length of the two lists is a well-founded relation.

So first we are going to define a relation about the lengths of two lists, and
convince our compiler that the relation is indeed well-founded.

```haskell
LengthLT : {a : Type} -> List a -> List a -> Type
LengthLT xs ys = LT (length xs) (length ys)

wfLengthLT : {a : Type} -> wellFounded (LengthLT {a})
wfLengthLt = ...
```

I omit the proofs here, but if you are interested you
can find them [here](https://github.com/VinaLx/playground/blob/master/WellFoundedInd.idr)

Then the implementation for quicksort follows:

```haskell
quickSort : Ord a => List a -> List a
quickSort {a} = wfRec {P = const (List a)} quickSort' wfLengthLT
  where
    quickSort' : (xs : List a)
              -> ((ys : List a) -> LengthLT ys xs -> List a)
              -> List a
    quickSort' [] _ = []
    quickSort' (x :: xs) rec
      =  rec (filter (\v => v <= x) xs) (LTESucc (filterSmaller _))
      ++ [x]
      ++ rec (filter (\v => v > x) xs) (LTESucc (filterSmaller _))
```

Unlike the complex-looking types of the fixpoint operator and quicksort itself,
the actual definition is rather clean. We delegate the implementation of quicksort
to a application of the well-foundedness recursive operator `wfRec`. Like using a
conventional fixpoint operator, we accept the recursive version of the function
as a parameter (`rec`), but here the recursive call is guarded by the condition
that the argument have to be shorter than the argument of parent call (`LengthLT ys xs`).
And we have to provide the proof of such relation holds upon making the recursive
call (`LTESucc (filterSmaller _)`). In this case we guarantee that the whatever
comes out of the `filter` must not be longer than whatever goes in (idris
generously provide exactly what we need in the library (`filterSmaller`)), hence the
argument of the recursive call is at least shorter by one than the argument of
the caller. Then the rest is only that you convincing yourself calling the `rec`
is indeed making a recursive call.

Although it looks like we are doing exactly the same thing as in the intro, this
definition perfectly conforms the structural restriction, hence is guaranteed by
the compiler that it terminates with all possible inputs. Quite impressive isn't it?

### The Mystery of Structural Recursion

Apparently we achieve all these with the conventional structural recurison, but
if you looking carefully at the definition of our essential operator:

```haskell
accRec : {P : a -> Type}
      -> ((x : a) -> ((y : a) -> r y x -> P y) -> P x)
      -> {x : a} -> Acc r x -> P x
accRec rec (MkAcc x f) = rec x $ \y, ryx => accRec rec (f y ryx)
```

The argument of the recursive call is `f y ryx`. How is it a sub-structure of
`MkAcc x f` when it's a return value of a function call? And more importantly
this definition doesn't look terminating when there is only one case, and it's
a recursive case.

Well, it definitely should terminate as that's what mathematics tells us.
~~The most straightforward way to look at it is that it's an generalized inductive
type (or reflexive data type) that the function represents a mapping from an
generalized index to a sub-structure.~~ But probably easier to understand, another
way is realizing that the function is not arbitrary, it comes from a correctly
constructed value of inductive types, which is obligated to find a function
that correctly construct the "sub-structure" whose procedure has to be finite
(with the structural restriction enforced). So in the end, it _will_ go down
to the end of the chain, where any further descend will lead to a
contradiction. For example when constructing (Acc (<) 0), it's impossible to find
any natural number `a` such that `a < 0`, hence the definition of
`(a : Nat) -> a < 0 -> Acc (<) a` is trivial.

That's also how a (seemingly) single case of recursion covers both base case
and recursive case. When reaching the end of the chain, where there is no `y`
such that `r y x`, `r y x` is isomorphic to $$ \bot $$ (a type with no value),
so whatever we do when constructing `(y : a) -> r y x -> P y` is trivial
since it's impossible to find its arguments and the recursion will never be called.