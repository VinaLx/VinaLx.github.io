---
layout: post
modified: 2017-09-08
title: Haskell Monad Basic - 2
excerpt: value and context, basic examples on List, Maybe and Either
category: [haskell, functional programming, monad]
---

In the last post we went through some most basic concepts of Monad in haskell, and sequencing simple IO operations with `do` notation. In this article I would present some other simple and common Monad instances and examples like `List`, `Maybe` and `Either`.

## Monad as the Computation Context

in the last article we introduce one (or two) method of Monad, the `(>>=)` (bind) operation. Actually there is a simple but equally important method of Monad, namely `return`,

~~~ haskell
class Applicative m => Monad (m :: * -> *) where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a
    -- ... other methods ...
~~~

which construct a "trivial" monad object with some certain value. Note that different from many imperative languages, `return` is not a keyword in haskell but a ordinary function. But it's usage expains its name very well, as we will see.

Now we take one step forward, we see monad as a context for value computation or transformation and take some extra _effects_. With this view, `return` is a way that _lifts_ an value from our world to certain monad context. It's not trivial as it looks like at all.

And about (>>=), it says that given a monad object of type `a`, and a way to construct a context of `b` with some plain value `a`, the monad can somehow retrieve the value in the original monad and feeds it into the function, usually during this process, some extra operation featured by the monad would be performed, like `IO` operation.

We will talk about the "context" and "value" concepts in more detail in the next article.

## Examples

### List

List Monad emphasize less on sequencing some operation but more on the "context" since it's easier to understand this way.

The list context represents 0 or more value of certain type. The `return` operation is simple, which simply construct a singleton list with that value. For (>>=), since every single value in the context (should) produce a new context, the result becomes the concatenation of every new context produced, that is it allow us to extend (or shrink) the original list on demand, according to the value.

Let's look at an trivial example first:

~~~ haskell
duplicate :: a -> [a]
duplicate a = [a, a]

copyEach :: [a] -> [a]
copyEach xs = xs >>= duplicate
~~~

if we test it in GHCi

~~~ haskell
𝝺 > copyEach [1,2,3,4]
[1,1,2,2,3,3,4,4]
~~~

This looks boring. The following may looks more interesting and useful. We can use (>>=) to implement `filter`,

~~~ haskell
filter :: (a -> Bool) -> [a] -> [a]
~~~

which says we leave all values that satisfy the predicate (a -> Bool), and filter out those that don't. We _can_ define the filter in this way

~~~ haskell
filter p xs = do
    x <- xs
    if p x then return x else []
~~~

Note that in this do notation, what we extract as `x` is **every value** in the list context. So for every value, if `p x` returns `True`, we preserve it, otherwise in the new context we just throw it away.

The list monad is useful also when we are processing more than one list, such as the computing the Cartesian Product of two list.

~~~ haskell
product :: [a] -> [b] -> [(a, b)]
product xs ys = do
    x <- xs
    y <- ys
    return (x, y)
~~~

It looks simple and clean, but also interesting to think about how it exactly works.

#### List Comprehension

Since list processing is quite common in haskell, haskell provides an syntactic sugar for the monadic operation on list.

The filter and product can be defined as :

~~~ haskell
filter p xs = [x | x <- xs, p x]
product xs ys = [(x, y) | x <- xs, y <- ys]
~~~

More detailed syntax of list comprehension can be found on [haskell wiki](https://wiki.haskell.org/List_comprehension)