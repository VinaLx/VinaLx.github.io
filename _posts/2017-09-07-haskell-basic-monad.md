---
layout: post
title: "Haskell Monad Basic - 1"
excerpt: motivation, introduction, definition of Monad and the "do" notation
category: [haskell monad basic]
modified: 2017-09-12
---

This series of articles would be basically my personal study reviews.
I would present an overview about monad, and hopefully make it less dreadful as its name looks like for new haskell or functional programming learners.

I assume readers to have a basic grasp of haskell syntax and a liiiittle experience in programming in functional style, but no math background would be required.

# Let's get started

`Monad` is maybe the most important programming pattern in haskell, the name "Monad" came from mathematic, but it (maybe) doesn't really matter that a programmer doesn't understand what its exact mathematical definition is, so do many other names we came across in haskell.

## Motivation

Monad exists everywhere in haskell, we met it when we typed the first (two) line of haskell program

~~~ haskell
main :: IO ()
main = putStrLn "Hello World"
~~~

Usually textbook would say we use `IO a` whenever we want to interact with the outside world. But that's not the whole story, IO itself is an instance of the mysterious Monad typeclass. Haskell add the syntactic sugar with monad called the "do" notation, with which we would be able to write program like

~~~ haskell
main :: IO ()
main = do
    putStrLn "what's your name?"
    name <- readLn
    putStrLn ("hello " ++ name)
~~~

We would delve into this syntax later in this article.

## Review of typeclass

Type class is a feature of haskell type system which provides a way to describe a class (set) of types which have similar operations. The simplest case the `Num` typeclass:

~~~ haskell
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  -- other "methods"
~~~

It basically says: if some type `a` has the following operations (`+`, `-`, `*`) defined, then we can treat it as a `Num` (number), and we would be able to write more general function using the `Num` typeclass instead of a specific type which represents some kind of number (`Integer`, `Double` etc.). And not surprisingly, `Integer`, `Double` types are _instances_ of the `Num` type class and define their arithmetic operations.

Recall that `Monad` in haskell is actually a typeclass, which says that: if some type have some Monad like operations (and follow certain rules, which cannot be enforced by syntax though), then we can call it a monad. And that's actually the case, we have many monad instances defined in haskell basic library.

## the Monad definition

So let's look at the definition of monad typeclass.

~~~ haskell
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a
  {-# MINIMAL (>>=) #-}
~~~

Leaving the `Applicative` aside, which would be explained in later series, the `{-# MINIMAL (>>=) #-}` is a pragma that tells the compiler an instance of Monad should define at least `(>>=)` and other methods work well automatically (thanks to the default method implementation of Monad). So in no doubt this `(>>=)` (pronounced as bind) operation is the core of Monad.

Sadly the type signature of `(>>=)` hardly makes sense for new comers. We leave the insight for now. Actually, the magic `do` notation we saw in early examples is basically a syntactic sugar of nested `(>>=)` operations, let's see how it works.

## Interact with `do`

Recall our simple example above:

~~~ haskell
main :: IO ()
main = do
    name <- readLn
    putStrLn ("hello " ++ name)
~~~

We said previously that `IO` is an instance of Monad, and readLn and putStrLn produces `IO`, here we have:

~~~ haskell
readLn :: Read a => IO a
putStrLn :: String -> IO ()
~~~

and here the type parameter `a` for read should (obviously) be `String`. And what we got is:

~~~ haskell
readLn :: IO String
putStrLn :: String -> IO ()
main :: IO ()
~~~

Oops, something familiar pops out. If we replace the `m` by `IO` in the signature of `(>>=)`

~~~ haskell
(>>=) :: IO a -> (a -> IO b) -> IO b
~~~

According to the type, it's rather easy to rewrite the program without the do notation

~~~ haskell
main :: IO ()
main = readLn >>= \name -> putStrLn ("hello " ++ name)
~~~

And this representation using `(>>=)` is equivalent to the code using `do` notation. Although I only give an `IO` example here, the `do` notation works for every other instances of monad as well, like `Maybe`, `List` and so on, like:

~~~ haskell
addIfPresent :: Maybe Int -> Maybe Int -> Maybe Int
addIfPresent mx my = do
    x <- mx
    y <- my
    return (x + y)
~~~

which is expanded to

~~~ haskell
addIfPresent mx my = mx >>= \x -> my >>= \y -> return (x + y)
~~~

We would give more simple examples in the next post. And now we introduce the general cases of desugaring `do` notation.

~~~ haskell
-- do notation
do
    v <- m
    expr
-- where "expr" is the same type of monad as "m", may contains v

-- will be expanded to
m >>= \v -> expr

-- if there are more than two expression
do
    v1 <- m1
    v2 <- m2
    v3 <- m3
    expr

-- will be expanded to
m1 >>= \v1 ->
    m2 >>= \v2 ->
        m3 >>= \v3 -> expr

-- what if no value is extracted?
do
    expr1
    expr2
    expr3

-- could be expanded to
expr1 >>= \_ ->
    expr2 >>= \_ -> expr3

-- or simply, recall the method (>>) of Monad
expr1 >> expr2 >> expr3
~~~

There are other cases like using `let` or pattern matching within `do`, those syntax are much simpler to understand.

the [Monad chapter of Real World Haskell](http://book.realworldhaskell.org/read/monads.html) has a more detailed introduction about `do` notation.

## Conclusion

Now what we know about `(>>=)`(or Monad) is that the only purpose of monad is for enabling the `do` notation and greatly improve the readability and simplicity of functional programs. And we can use a imperative programming style while still using pure functional language like haskell. (with certain monad we can even make local variable mutable!).

In this article we introduce an imprecise, simplified view of Monad, how it relates to the ordinary `do` notation. And in the following series I will first go through some more simple examples of Monad in haskell, try to give an insight of how monad is introduced, and some more not-so-simple topics.