---
layout: post
title: "Haskell Monad Basic - 4"
excerpt: Reader, Writer and State (Unfinished)
category: [haskell monad basic]
modified: 2017-11-25
---

In this post we introduce three moands that are (also) constantly used in real life. `Reader`, `Writer` and `State`. They somewhat have different taste from the monad we used to know, `List`, `Maybe` etc, whose contexts emphasize more on the amount of "values" inside of it. While in this post, we talk more about the "environment"s in which our program (well, monad actually) is running.

They are all implemented in haskell base library and "extended" in the `mtl` (Monad Transformer Library), we would introduce a simplified version of all three, and care about the monad transformer in the next post.

## Reader

Programs we wrote often have some degrees of "global configuration", command line arguments, content of configuration files, or other in a more abstract concept. In this case the behaviour of our program depend on the environment, we may not want (or need) to change or modify them.

In object oriented language style, we encapsulate such environment inside an object, and we can use the value of environment freely inside the object, or we simply use a "global variable" or something alike. but obviously, we aren't able to set the value of global variable in purely functional programming language. So what do we do now? Of course we can pass the environment around in our functions as a parameter, but this has a serious problem that every function of our program get a same extra parameter slot, and program would soon becomes clumsy and hard to maintain.

And `Reader` is doing exactly abstracting out this process as a monad. It's defined like this (**not exactly**, we will talk about it in the next post):

``` haskell
newtype Reader r a = Reader { runReader :: r -> a }
```
(I won't explain the `newtype` keyword and the record syntax of haskell, if you don't know, google them **right now**)

The `Reader` is essentially a function that mapping some configuration `r` and getting a result `a`. If you are familiar with the moand instance of `(->)`(the function type), you find it easy to understand `Reader`. Well, but here I assume you don't. So let's first see how `Reader` becomes a monad. Alike to the `Either` with take two type parameters, `Reader` is a monad (and functor and applicative) with the first type parameter fixed.

The functor instance is straightforward, we mapping the result of the function (compose them) yielding a new function (new `Reader`, actually)

``` haskell
instance Functor (Reader r) where
    -- fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap ab (Reader ra) = Reader (ab . ra)
```

The applicative instance of `Reader` says nothing more after we know the monad of it (implement that yourself!), so we jump right into the monad.

``` haskell
instance Monad (Reader r) where
    -- const :: a -> r -> a
    return a = Reader (const a)

    -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader ra) >>= f = Reader $ \r -> runReader (f $ ra r) r
```

Things begin to look interesting. The first question is "what does `return` do"? There aren't many choices left to us. Since we know nothing about `a` and we can do nothing but return the `a` we get in the reader, which says: "`return` gives us the reader that ignore the environment and yielding constant value" Seems boring, we'll see.

And `(>>=)` looks again very horrible at first sight. But looking more carefully on that, we figure out the following.

- We are defining a function that takes `r` returning `b`, of course.
- Inside the function, we run the original reader, and get an `a`.
- We throw `a` into the bind function, getting another new reader about `Reader r b`
- We then run the new reader `b` with same environment `r` and get a value of `b`

Now we know how the immutable "configuration" is passing around multiple readers by `(>>=)`. But how do we actually make use of the `Reader` in our program? Suppose we want to calculate the time when human extinct with the help of the ultimate answer of the universe.

``` haskell
type AnswerReader = Reader Int

endOfHuman :: AnswerReader Int
endOfHuman = do
    ultimateAnswer <- getConfig -- how to do this?
    return $ doSomeSeriousCalculation ultimateAnswer

prophesy :: AnswerReader String
prophesy = do
    y <- endOfHuman
    return $ printf "human extincts before %d" y
```

So how do we get the configuration inside of `do`? Notice that on the left-hand of the `<-` is the result of the reader **after** applying some configuratio, and since all readers share the same configuration, we can construct a reader that tell us what configuration it received and we get the global configuration. How to do that?

```haskell
getConfig :: Reader r r
getConfig = Reader id

-- in base library and mtl, `getConfig` is called `ask`
ask :: Reader r r
ask = getConfig
```

And there're other utilities of `Reader`, called `local` and `asks`, try to implement them yourself:

``` haskell
ask :: Reader r r
ask = Reader id

-- | modified the environment _locally_ in given reader only
local :: (r -> r) -> Reader r a -> Reader r a

-- | similar to ask, but map the config to another value
asks :: (r -> a) -> Reader r a
```
