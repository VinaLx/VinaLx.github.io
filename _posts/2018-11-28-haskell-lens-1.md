---
layout: post
title: "Lens - Exploring and Learning - 1"
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

But wait, it's not really necessary to always set the sub-object to a value of the same type,
which is quite obvious in this tuple example.

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

Another problem of this abstraction is, as the type name suggest, it's a setter **and** a getter.
But it's not very often we want them both **simultaneously**. When we're calling "get", we don't need the setter at all, when we're calling "set", we don't need the getter at all.

### GetOrSet

Exploiting the functor operation, we can use `Either` instead.

```haskell
type GetOrSet r s t a b = (a -> Either r b) -> s -> Either r t

getOrSetFst :: GetOrSet r (x, y) (z, y) x z
getOrSetFst xrz (x, y) = flip (,) y <$> xrz x -- exact same implementation
```

Now it can behave like a setter or a getter depending on the situation, we are getting there!

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

## Type Refinement

Great, but the use of `getLeft` and `getRight` here is annoying, calling an untotal function in functional programming always isn't a good idea.
But since we know that we will be getting out `Left` if we produce a `Left` and vice versa, we are fine as long as the implementation of `GetOrSet` is reasonable.

But anyway, if we think about it, `get` always only use the `Left` and `set` always only use the `Right`, let's step back and try to express that in the type signature of `get` and `set`.

### `Const`, the functor that does nothing

`fmap`ing an `Either` contains a `Left` doesn't do anything. Is there a functor that contains only the `Left` case of `Either`? There is! The `Const` functor:

```haskell
-- Data.Functor.Const
newtype Const a b = Const { getConst :: a }

instance Functor (Const a) where
    fmap _ (Const a) = Const a
```

That type looks really dumb at first glance, but this turns out to be exactly what we need now! So that the `get` becomes:

```haskell
type Getter r s t a b = (a -> Const r b) -> s -> Const r t

get :: Getter a s t a b -> s -> a
get getter = getConst . getter Const
```

Let's move on to `set`

### `Identity`, the functor of "no functor"

Ignoring the `Left` part of `Either` means ... Eh, if we look back, we don't really need any functor there, `(a -> b) -> s -> t` is just fine. But if we really want to add one, for the greater good... I would say `Identity`.

```haskell
-- Data.Functor.Identity
newtype Identity a = Identity { getIdentity :: a }

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

type Setter s t a b = (a -> Identity b) -> s -> Identity t

set :: Setter s t a b -> (a -> b) -> s -> t
set setter f = runIdentity . setter (Identity . f)
```

### `Lens`

Now, the instances of `Functor` used by `Setter` and `Getter` is the only difference in the type alias definitions. We could extract it with another type alias:

```haskell
type LensLike f s t a b = (a -> f b) -> s -> f t

type Setter   s t a b = LensLike Identity  s t a b
type Getter r s   a   = LensLike (Const r) s s a a
```

However, this way we lose the information that `f` is required to be a `Functor`,
but we can NOT add a type class constraint to the `type` alias definition like `type Functor f => Lens f ...`.

What we could make use of the [`RankNTypes`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#arbitrary-rank-polymorphism) language extension, and completely abstract out the type parameter
in the head of the declaration to the body of type like so:

```haskell
type Lens s t a b = forall f. Functor f => LensLike f s t a b

-- `setOrGetFst` with exactly the same implementation
_1 :: Lens (x, y) (z, y) x z
_1 f (x, y) = flip (,) y <$> f x

-- same definition as above
set :: Setter s t a b -> (a -> b) -> s -> t

set' :: Setter s t a b -> b -> s -> t
set' setter = set setter . const
```

With `RandNTypes`, we don't even mention the type parameter `f` in `_1`,
because the `f` is abtracted out directly in the type, and is no longer a
"type variable" of the type alias. And we could still use a `Lens` as a `Setter`
or `Getter` like so:

```haskell
𝝺> set _1 (+1) (1, 'a')
(2,'a')

𝝺> get _1 (+1) (1, 'a')
2
```

During type checking, ghc notices that the type `forall f. Functor f => (a -> f b) -> (s -> f t)`
is compatible to both type `(a -> Identity b) -> (s -> Identity t)` and `(a -> Const r a) -> (s -> Const r s)`,
because both `Identity` and `Const` are functors, which satisfy the `Functor` constraint in the `forall` quantified type.

## Back to Reality

Now we "almost" achieve what we saw before, we have a clean design of `Setter` and `Getter` type and both of which related to `Lens`, we didn't quite obey the naming in the implementation in the library, and some types have _slightly_ different signatures.

The actual type signatures are defined in

- [`Control.Lens.Getter`](http://hackage.haskell.org/package/lens-4.18.1/docs/Control-Lens-Getter.html)
- [`Control.Lens.Setter`](http://hackage.haskell.org/package/lens-4.18.1/docs/Control-Lens-Setter.html)
- [`Control.Lens.Lens`](http://hackage.haskell.org/package/lens-4.18.1/docs/Control-Lens-Lens.html)

```haskell
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

type LensLike f s t a b = (a -> f b) -> s -> f t
type LensLike' f s a = LensLike f s s a a

-- `Settable` constraint is mostly talking about `Identity`
type Setter s t a b = forall f. Settable f => (a -> f b) -> s -> f t
type Setter' s a = Setter s s a a

type ASetter s t a b = (a -> Identity b) -> s -> Identity t
-- `set` in our article
over :: ASetter s t a b -> (a -> b) -> s -> t
-- `set'` in our article
set :: ASetter s t a b -> b -> s -> t

-- `Getter` in our article
type Getting r s a = (a -> Const r a) -> s -> Const r s
-- again, the constraint is mostly talking about `Const`
type Getter s a = forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s

-- a simple "getter"
(^.) :: s -> Getting a s a -> a

-- some magic combined with (.^) to obtain a behavior of `get` in our article
to :: (Profunctor p, Contravariant f) => (s -> a) -> Optic' p f s a
```

## Conclusion

In this article, we kinda "derived" the design of `Lens` informally, and in the next article
we would explore something left by this article, the composition of lenses,
and some magic constraints (like `Contravariant`) in the real implementation.