---
layout: post
title: Haskell Monad Basic - 3
excerpt: "Monad and its \"super classes\""
modified: 2017-09-13
category: [haskell, functional programming, monad]
---

Recall the definition of the monad typeclass:
~~~ haskell
class Applicative m => Monad (m :: * -> *) where
    -- methods...
~~~

After two articles, there are still something unsettled: what is the `Applicative` here? To understand this one, we first have some quick review on the typeclass syntax of haskell.

## "Inheritance" of typeclass

In haskell, we can declare an "inheritance" relation over one or multiple typeclasses using type constraint. Note the "inheritance"s here are always quoted, since it just meant to make beginners with object oriented background easier to understand, and we are not really meaning the "inheritance" in the OO system.

For example, the `Ord` typeclass

~~~ haskell
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  {-# MINIMAL compare | (<=) #-}
~~~

Here we basically say "If we want to declare an instance of `Ord` for type `a`, there must be an instance for `Eq a`". So knowing the existance of `Eq a`, we can define default implementation of `Ord` using methods in `Eq`. The whole thing works as if the `Ord` interface is a "subclass" of `Eq` and "inherit" all the methods in `Eq`. And we say, "if `a` is an `Ord`, `a` must be an `Eq`".

And the same thing apply to `Monad` as well, so the definition above can be read as, "if `a` is a `Monad`, `a` must be an `Applicative`"

## the "inheritance chain" for `Monad`

OK then, if we look at the definition of `Applicative`, we find an upstream of `Applicative` as well, the `Functor`. And we copy and paste the definition of them here.

~~~ haskell
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}

class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
~~~

Again we are overwhelmed by all the mysterious type signatures here. Don't panic, the "inheritance" relation tells that that, the `Functor` must be the most basic one, `Applicative` extends some ability of `Functor`, and `Monad` is even more powerful than `Applicative` in some way.

In the [last post](/articles/2017-13/haskell-basic-monad-2) we introduced an idea of "context" and "value". And as we will see, the power extended over the "inheritance chain" is the capability of manipulating the structure of "context". And then hopefully the signature of those operation makes more sense to you.

## `Functor` for value transformation

The term `Functor` comes from category theory. But in functional programming, functor only means one thing:

~~~ haskell
fmap :: Functor f => (a -> b) -> f a -> f b
~~~

which says with f being a functor, if we know how plain value is mapped to another(`a -> b`), we know how values under the context are mapped (`f a -> f b`).

For example:

~~~ haskell
𝝺 > :t toUpper
toUpper :: Char -> Char

𝝺 > :t fmap toUpper
fmap toUpper :: Functor f => f Char -> f Char

𝝺 > fmap toUpper "hello functor"
"HELLO FUNCTOR"
~~~

that is, we _lift_ the transformation of values to the functor context, such as the `list`, the `maybe` etc. And that's all what functor can do.

The restriction is that the context itself doesn't change at all during the mapping, the length of list doesn't change, `Just` is still a `Just`, `Nothing` is still `Nothing`. This makes sense because what we specified is only how plain values are mapped and know nothing abount the context at all.

And the major limitation of functor is that we can only manipulate one context at the same time, we can map only one functor object to another, but we can't even do the following with functor.

~~~ haskell
-- NO WAY!
map2 :: Functor f => (a -> b -> c) -> f a -> f b -> f c
~~~

The rationale is that we just promise that we never change the context of functor, but two functors may have different contexts, then what is the context of the result? Functor just doesn't know.

Careful readers must notice that this operation is provided by the `Applicative`, which gives us some ability to "change" the contexts we have.

## `Applicative` for combining contexts

Applicative functor, or simply `Applicative` got it name for the ability to apply function under context, that is

~~~ haskell
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- compare to
$ :: (a -> b) -> a -> b
~~~

When applying function under an context, the applicative should merge two contexts accordingly (`f (a -> b)` and `f a`). And `pure`, the applicative version of `return` is for generating the default context without extra effects.

After we have some ability to combining context, a lot of interesting things can happen, here we only go through the most simple and basic ones.

Note that `(<*>)` and `liftA2` are actually equivalent, each of them can be implemented with the other

~~~ haskell
class Functor f => Applicative (f :: * -> *) where
  (<*>) :: f (a -> b) -> f a -> f b
  (<*>) = liftA2 ($)
  liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f
  liftA2 f fa fb = pure f <*> fa <*> fb
~~~

For lists, the length of the list becomes multiplication of length of two lists of parameter

~~~ haskell
product :: [a] -> [b] -> [(a, b)]
product = liftA2 $ \a b -> (a, b)

𝝺 > product [1,2,3] ['a', 'b', 'c']
[(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c'),(3,'a'),(3,'b'),(3,'c')]
~~~

For `Maybe`s, applicative give us the ability to propagating `Nothing`

~~~ haskell
addIfPresent :: Maybe Int -> Maybe Int -> Maybe Int
addIfPresent = liftA2 (+)

𝝺 > addIfPresent (Just 1) (Just 2)
Just 3
𝝺 > addIfPresent (Just 1) Nothing
Nothing
~~~

With applicative functor, we are now able to create new context out of old contexts, but is that enough? No! With only applicative, we are not even able to implement many ordinary useful list manipulation functions like `filter`, or sequencing operations that can go wrong (accept values and return `Maybe`).

Why is that? Since applicative only gives us the ability to create contexts in terms of contexts and nothing to do with the value inside, observing that the context of the result is fully determined by the contexts of parameters, no matter what value they contain. So we are not able to generate contexts according to values, but functions like `filter` have to look at values inside the context and determine the resulting context.

And you know what is coming to help, the `Monad`.

## `Monad` for generating context

Now the intention of `(>>=)` is completely clear for us

~~~ haskell
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  -- ...
~~~

That's exactly what we want, generating new context according to the value inside the first parameter. We presented many examples in the earlier posts and you can feel the power of monad.

Amazingly, the power of `(>>=)` is not even a supplement of `Applicative` and `Functor`, we can implement `fmap` and `<*>` by only monad interfaces!

Let's try it:

~~~ haskell
-- monad implement version of ...
fmap :: Monad m => (a -> b) -> m a -> m b
fmap f ma = ma >>= return . f

<*> :: Monad m => m (a -> b) -> m a -> m b
mf <*> ma = mf >>= \f -> ma >>= \a -> return f a
~~~

You may feel something uncomfortable: yes our program pass type checks if we implement `fmap` and `(<*>)` this way. How do we guarantee that they actually mean the same thing? In practice we can't, of course, no one can stop you from implementing those interfaces in a strange way. But "theoretically" we can. Since these typeclasses don't come from nowhere, their interfaces should satisfy some "laws" then can we actually call them `Functor`, `Applicative`, or `Monad`. And then we can prove, different implementation of `fmap` and `<*>` are equivalent.

## Laws for the `Monad` family

The laws came from the original definitions or property in maths. I won't talk about math theory, but simply present all of them here and only get you some basic feelings.

### `Functor` law

~~~ haskell
-- identity
fmap id = id

-- the distributive
fmap (g . f) = fmap g . fmap f
~~~

Basically these laws guarantee that the value transformation relation (identity and function composition) doesn't change after applying some context `f`.

### `Applicative` laws

~~~ haskell
-- identity
pure id <*> a = a

-- associativity
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

-- homomorphism
pure f <*> pure a = pure (f a)

-- commutativity
u <*> pure a = pure ($ a) <*> u
~~~

Note that the applicative laws is mainly restricting the context generated by `pure`.

Two pure context should combine to another pure context (homomorphism).

Applying pure context to another context doesn't change that (commutativity).

The evaluation order of a sequence of contexts combination doesn't change the resulting context (associativity law).

### `Monad` Laws

~~~ haskell
-- left identity
return a >>= f = f a

-- right identity
m >>= return = id

-- associativity
(m >>= f) >>= g = m >>= (\x -> f x >>= g)
~~~

Again, left and right identity restrict the context that `return` can generate.

The associativity law looks a lot more complicated. But the name of it explains very well here. As long as the order of context generation is fixed, the resulting monad object should be the same.

## Conclusion

In this article we saw the increasing power of context manipulation along the "inheritance chain" of `Monad`. I use the "context" and "value" term all along only for get you some intuition (hopefully) about what what those interfaces are doing, although it may be quite imprecise or completely nonsense in the view of mathematicians :P.

In the end we present the "laws" that the implementations should follow, although no one including compiler cannot stop you from breaking the laws, but in case of getting strange behavior of your monad, following those laws when implementing your own monad instance is considerably a good practice.

There are a lot of things we didn't cover here. To know more about `Applicative` or `Functor`, [this article in haskell wikibook](https://en.wikibooks.org/wiki/Haskell/Applicative_functors) is a good place.

Next article we will go deeper in some pratical monads in haskell monad library and introduce another important concept called the "Monad Transformer". But before that, you may want to get yourself more familiar with the operation of basic monads and understand how they work.
