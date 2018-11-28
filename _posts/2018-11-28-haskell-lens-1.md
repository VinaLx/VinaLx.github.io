---
layout: post
title: "[WIP] Lens - Exploring and Learning - 1"
excerpt: Introduction / Abstractions of "setter"s and "getter"s.
category: ["haskell", "lens"]
modified: 2018-11-28
---

I learned lens about 6 months ago and I thought it an fascinating concept. The idea, the abstraction, the implementation and the final DSL it achieve are exciting. But also, it's very complicated not only by itself, but also the related topics of functional programming and theory. Now I almost forgot everything but the thrill of learning it. Now I want to re-learn this beautiful thing and try not to forget that again. So here come this series of post.

To be honest, the theories behind _lens_ is way beyond my current capability, so there's a high chance for me to make mistakes, explain subtle topic inaccurately or imprecisely through these upcoming posts. So please point them out (by email or whatever way to contact me) should that happen, I would appreciate it very much!

Now onward!

## What is "Lens"

"Lens"es are generally considered to be "functional references" to sub-objects inside "parent" objects, such as a data field of a (object-oriented) class, the second element of a two-tuple, 4th element in a list etc. Lens push this idea to a more general perspective, we can say "every element that's equal to one", "sum of the elements in the list", "reverse view of the string itself". Moreover, Lens solves this problem in a purely functional, composable, therefore elegant way.

Let's look at several examples from [official repository of haskell implementation](https://github.com/ekmett/lens):

_assuming `_1`, `_2` are lenses referencing to the first and the second field of an tuple_

```haskell
-- get
ghci> ("hello","world") ^. _2
"world" --"get" operator ^

-- set
ghci> set _2 42 ("hello","world")
("hello",42)

-- composition
ghci> ("hello",("world","!!!"))  ^. _2 . _1
"world" -- function composition itself ^

-- manipulation
ghci> ("hello",("world","!!!")) ^. _2 . _2 . to length
3
```

It looks amazingly clean syntax-wise, but it's a little too clean for now.
Ignoring many other complexities, lenses here seem to act like a combination of "getter" and "setter", so that they can answer the corresponding request they've asked to.
Of course that's the intuition of "sub-object reference" after all -- just retrieving the value or modifying them.
So let's look at these two "component"s of lens separately and try to combine them together.

## Abstractions

### Getter

This is a relatively simpler one, who doesn't know how to access a value. Recalling getting element out of a tuple.

```haskell
fst :: (a, b) -> a
snd :: (a, b) -> b
```

That's simple enough, but we probably want more generality. What about:

```haskell
type Getter s a = s -> a

fst :: Getter (a, b) a
snd :: Getter (a, b) b
```

Now we are mostly good, but we don't always want whatever we're getting out, we probably want to preserve the option to transform the result to some other thing as the result.
We certainly can do that with function composition, but we have another way to do that without the help of the compose operator. Let's add some [CPS](https://en.wikipedia.org/wiki/Continuation-passing_style) taste to our `Getter`.

```haskell
type Getter r s a = (a -> r) -> s -> r

getFst :: Getter (a, b) a c
fst = getFst id

getSnd :: Getter (a, b) b c
snd = getSnd id
```

That's better, let's move on to our setter.

### Setter

Let's first define our tuple setters.
Of course in a pure language as haskell, we cannot directly "set" the original object, the common pattern is that we return a new copy instead.

```haskell
setFst :: a -> (a, b) -> (a, b)
setSnd :: b -> (a, b) -> (a, b)
```

The first thing we notice is that we don't always have the value to set, we usually want to "update" the object depending on the original value. Let's fit this idea into our setter.

```haskell
type Setter s a = (a -> a) -> s -> s

setFst :: Setter (a, b) a
setSnd :: Setter (a, b) b
```

But wait, do I always have to set the sub-object to the same type?
In this tuple example, the problem become quite obvious, let's fix that.

```haskell
type Setter s t a b = (a -> b) -> s -> t

setFst :: Setter (a, b) (c, b) a c
setFst ac (a, b) = (ac a, b)

setSnd :: Setter (a, b) (a, c) b c
setSnd bc (a, b) = (a, bc b)
```

## Getter + Setter

Next step, let's put getter and setter together. Recall our `Getter` and `Setter` definition

```haskell
type Getter r s a   = (a -> r) -> s -> r
type Setter s t a b = (a -> b) -> s -> t
```

Note that, one thing that trapped me is, although at first glance `a -> r` and `a -> b` looks rather similar, but they convey different meanings.
`r` is just the whatever result returned by the continuation, but `b` is the actual thing we want to set to `s`.

So how can we stick them together? Although `r` and `b` don't mean the same, but two types still share a very similar structure. Our first try would be:

### GetAndSet

```haskell
type GetAndSet r s t a b = (a -> (r, b)) -> s -> (r, t)

getAndSetFst :: GetAndSet r (x, y) (z, y) x z
getAndSetFst xrz (x, y) = let (r, z) = xrz x in (r, (z, y))
-- or using the fact that `(r, )` is a functor
getAndSetFst xrz (x, y) = flip (,) y <$> xrz x
```

It works, but clearly it doesn't look very elegant.

Interestingly enough, one thing we observed above is that `(r, b)` is actually a functor with respect to `b`. Why does that matter?
Because now `(r, b) -> (r, t)` suddenly becomes a functor operation (`fmap`), which indicates that what we don't really have to break down the tuple `(r, b)` to get the final result, we just care about the `b -> t` part of it.

Another flaw of this abstraction is, as the type name suggest, it's a setter **and** a getter.
But it's not very often we want them both **simultaneously**. When we're calling "get", we don't need the setter at all, when we're calling "set", we don't need the getter at all.

### GetOrSet

Exploiting the functor operation, we can use `Either` instead.

```haskell
type GetOrSet r s t a b = (a -> Either r b) -> s -> Either r t

getOrSetFst :: GetOrSet r (x, y) (z, y) x z
getOrSetFst xrz (x, y) = flip (,) y <$> xrz x -- exact same implementation
```

Now it can behaves like a setter or a getter depending on the situation, we are getting there!

```haskell
getLeft :: Either a b -> a
getLeft (Left a) = a

getRight :: Either a b -> b
getRight (Right b) = b

get :: GetOrSet a s t a b -> s -> a
get gos = getLeft . gos Left

set :: GetOrSet r s t a b -> (a -> b) -> s -> t
set gos ab = getRight . gos (Right . ab)
```

Let's try it:

```haskell
𝝺 > get getOrSetFst (1, 2)
1
𝝺 > set getOrSetFst length ("abc", 2)
(3,2)
```

Perfect...? Or not. If you look at the type parameters of `get` and `set`, there're still redundancies on type signatures. If I'm using `get`, `t` and `b` is completely useless, similarly, there's certainly no point to write an `r` on the type when using `set`. What can we do to that?

## Type Refinements

Before doing anything, let's sort out what we want to do:

- Get rid of type `b` and `t` in the type of `get`
- Get rid of type `r` in the type of `set`
- Provide a consistent interface of `GetOrSet` on which `get` and `set` can call

### Magic of Functors

-- to be continued ...