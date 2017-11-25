---
layout: post
modified: 2017-09-08
title: Haskell Monad Basic - 2
excerpt: value and context, basic examples on List, Maybe and Either
category: [haskell, functional programming, monad]
---

In the [last post](/articles/2017-09/haskell-basic-monad) we went through some most basic concepts of Monad in haskell, and sequencing simple IO operations with `do` notation. In this article I would present some other simple and common Monad instances and examples like `List`, `Maybe` and `Either`.

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


### `Maybe`

#### motivation

the `Maybe` type is a super useful tool for type safe error handling. It's defined as:

~~~ haskell
data Maybe a = Just a | Nothing
~~~

which basically say an maybe object may or may not has a value of its type parameter. When we want to define some function that can go wrong, we make the function return an `Maybe` object and everyone knows that the function returns `Nothing` as a sign of error.

But something tricky remains, what do we do after getting a "maybe". If there's only one function returns maybe, we're fine, we simply pattern match it and divide the control flow. But considering the following senario.

~~~ haskell
-- Many steps of the computation can goes wrong...
actionA :: A -> Maybe B
actionB :: B -> Maybe C
actionC :: C -> Maybe D

-- Then how do we implement the action that combine actionA, B, C together
action :: A -> Maybe D

-- OK let's try pattern matching...
action a = case actionA a of
    Just b  -> case actionB b of
        Just c  -> case actionC c of
            Just d  -> Just d
            Nothing -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing
~~~

The senario is quite common in real life, the steps of the large computation can goes wrong individually. And as we see, pattern matching along the way is **definitely not** what we want here. But if we recognize the pattern of this pattern matching process, we find something familiar again.

#### the monad

If we have a function that do all pattern matching for us, that would be good.

~~~ haskell
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
~~~

And look at the pattern of the pattern matching above, we observe that when we want to convert some `Maybe` to another `Maybe`, the only way we success is all the computation success, if any of the middle steps goes wrong, we shutdown the whole process since further computation cannot be done.

So what monad can do for us is that

~~~ haskell
ma >>= f = case ma of
    Nothing -> Nothing
    Just a  -> f a
~~~

And when chaining things together, things go extremely fluent and clean here

~~~ haskell
actionRefined :: A -> Maybe D
actionRefined a = do
    b <- actionA a
    c <- actionB b
    actionC c
~~~

It seems unbelievable, but it's perfectly make sense once you figure it out. Just note that if actionA "goes wrong" and fail to produce some "b" here, actionB and the following would fail to continue as well.

What's extraordinary for maybe monad is that, when we are sequencing operation that may goes wrong, we can only care about the logic of going the right way, and let maybe monad handle the rest of that, shut the computation down at some correct time.

`Maybe` monad, like most kind of monads, emphasizes on sequencing operations within some context. We lift ordinary value to a context where error may happen, and every step of computation, the `Maybe` monad checks the presentation of error and decide whether the computation goes on.

### `Either`

But, despite the "perfectness" of `Maybe`, something is still unsatisfying. When an operation of a sequence of small operation returns `Nothing`, we have no way to know which step of the operation goes wrong outside, which maybe crucial in some senario, we want to produce some reasonable error messages for example.

The reason why this awkward thing happens is that, when error come out, the only way we can represent the error is `Nothing` and nothing else. If we can say more about `Nothing`, that would be good, and here's when `Either` can help.

~~~ haskell
data Either a b = Left a | Right b
~~~

Conventionally, we use `Right` to denote the success case, and `Left` to denote the error case. We can choose `String` or whatever type we want to represent errors.

You may wonder how `Either` can be a monad since it has literally two type parameters while monad requires the type has only one. The solution is we can fix the first parameter and make `Either` and the first paramter together a monad instance, and it is defined in this way:

~~~ haskell
instance Monad (Either e) where
    -- implementations...
~~~

And that tells us, basically if we want to handle error with `Either` in a monadic way, we have to fix the type for the "error" for a period of time (throughout the error handling code at least).

There's really nothing new for either monad compares to maybe monad, both monads are for functional style error handling. The last thing we should know about either monad is that, we said either monad can preserve **some** information about errors, it **only preserves the first error information it encounters**. It makes sense because after the first error, the rest of computation never continues just as what maybe monad do.

But either is still not perfect, if we are doing some operations that don't rely on the result of others, we can still preserve **only one** error message. And if we want to provide the functionality that can notify the receiver of the result all the error happens in a bunch of operations, new structure or design would be required.

#### reflection about error handling

At last we settle some subtle questions.

The first is the comparison of maybe and either monad, if either can always preserve more information than maybe do, why would we use maybe at all? The thing is that in many situations when error occurs, we can instantly know when errors should occur, as following examples

~~~ haskell
div :: (Num n) => n -> n -> Maybe n

index :: Int -> [a] -> Maybe a
~~~

When we are dividing some numbers and goes wrong, we know that in 90% of the cases that's because the divisor is zero. Similarly, when we index through an array like structure and an `Nothing` is returned, we know it's probably an out-of-range error. And there are many cases like these, if just `Nothing` itself is perfectly makes sense, why would we bother to look at those error messages or reasons.

The second question is why we use the horrifying monad to handle exceptions and errors, why can't we just throwing exception around and catch them at appropriate places like many imperative languages. This is techniquely not a topic about monad but something more higher level at the "ideas of functional programming". I don't want to talk very much about this right here, in short, not allowing function type signature to lie should be a good thing most of the time.

## Conclusion

We walk through some most basic and useful monads in haskell in this article, and rest of the thing for you just look at the documentation yourself and try to play with those monads in your codes. Although monads in this article themselves are not so complicated, but when "stacked" with other monads as would be introduced in future article, it will be a different thing, so it's important to master these simple things before going further.

In the next article, I will **try to** demystify what `Applicative` in the `Monad` definition and all other related things like `Functor`. I can't explain all the math and theoretical things about where are those terminologies come from, but hopefully give you a most basic idea about what those things are in a functional programming language.