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

### Motivation

Programs we wrote often have some degrees of "global configuration", command line arguments, content of configuration files, or other in a more abstract concept. In this case the behaviour of our program depend on the environment, we may not want (or need) to change or modify them.

In object oriented language style, we encapsulate such environment inside an object, and we can use the value of environment freely inside the object, or we simply use a "global variable" or something alike. but obviously, we aren't able to set the value of global variable in purely functional programming language. So what do we do now? Of course we can pass the environment around in our functions as a parameter, but this has a serious problem that every function of our program get a same extra parameter slot, and program would soon becomes clumsy and hard to maintain.

### the `Reader` Monad

`Reader` is doing exactly abstracting out this process as a monad. It's defined like this (**not exactly**, we will talk about it in the next post):

``` haskell
newtype Reader r a = Reader { runReader :: r -> a }
```
(I won't explain the `newtype` keyword and the record syntax of haskell, if you don't know, google them **right now**)

The `Reader` is essentially a function that mapping some configuration `r` and getting a result `a`. If you are familiar with the moand instance of `(->)`(the function type), you find it easy to understand `Reader`. Well, but here I assume you don't. So let's first see how `Reader` becomes a monad. Alike to the `Either` with take two type parameters, `Reader` is a monad (and functor and applicative) with the first type parameter fixed.

The `Functor` instance is straightforward, we mapping the result of the function (compose them) yielding a new function (new `Reader`, actually)

``` haskell
instance Functor (Reader r) where
    -- fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap ab (Reader ra) = Reader (ab . ra)
```

And here for the `Applicative`, we said in the previous post that `Applicative` is for "combining context", and that's exactly what we care about:

```haskell
instance Applicative (Reader r) where
    pure a = Reader (const a)
    -- (<*>) :: Reader r (a -> b) -> Reader a -> Reader b
    (Reader rf) <*> (Reader ra) = Reader $ \r -> rf r $ ra r
```

Things begin to look interesting. The first question is "what does `pure` do"? How do we generate a context out of none? There aren't many choices left to us. Since we know nothing about `a` and we can do nothing but returning the `a` we get in the reader, which says: "`pure` gives us the reader that ignore the environment and yielding constant value" Seems boring, we'll see.

And the `(<*>)` is for combining context of two reader, the context here is the "global configuration", so in the result, we define a new function taking the configuration, and then pass the configuration to both reader, and apply the function inside of it.

At last we come to the monad, which defining the behaviour of sequencing the operation.

``` haskell
instance Monad (Reader r) where
    return = pure
    -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader ra) >>= f = Reader $ \r -> runReader (f $ ra r) r
```

And `(>>=)` looks again very horrible at first sight. But it makes a lot of sense when you looking at it.

- We are defining a function that takes `r` returning `b`, of course.
- Inside the function, we run the original reader, and get an `a`.
- We throw `a` into the bind function, getting another new reader about `Reader r b`
- We then run the new reader `b` with same environment `r` and get a value of `b`

Now we know how the immutable "configuration" is passed along multiple readers by `(>>=)`. But how do we actually make use of the `Reader` in our program?

### Usage

Suppose we want to calculate the time when human extinct with the help of the ultimate answer of the universe.

``` haskell
type AnswerReader = Reader Int

endOfHuman :: AnswerReader Int
endOfHuman = do
    ultimateAnswer <- getConfig -- how to do this?
    return $ doSomeSeriousCalculation ultimateAnswer

prophesy :: AnswerReader String
prophesy = do
    y <- endOfHuman
    return $ printf "human extincts before %d A.D." y
```

and then we run the reader `prophesy`

```
𝝺 > runReader prophesy 42
"human extincts before 4242 A.D."
```

(not so serious calculation)

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

## Writer

Now we took the first step, then things would come easier. Then we look at `Writer`.

### Motivation

It's very common that a program continuoutly produces output while running, like logging or something informing the current status of the program. But since no "side effect" is allowed in functional programming, how do we produce the output? We have to define function that produce both the original result of the function, and the "log" the program. That maybe somewhat ok to you, but when we have a lot of function that all produce result and log, how do we accmulate the all the logs? Define a monad, sure.

### the `Writer` Monad

`Writer` is a type that combine the result of the program and the logs. Talking about "combine", nothing is simpler than a tuple, right? So the `Writer` is something like this:

``` haskell
newtype Writer w a = Writer { runWriter :: (a, w) }
```

Then we try to define the monad instance of `Writer`.

the `Functor`, again, it's trivial since we are only mapping the result.

``` haskell
instance Functor (Writer w) where
    fmap f (Writer (a, w)) = Writer (f a, w)
```

But when coming to "combining the contexts", we have some troubles now.

``` haskell
instance Applicative (Writer w) where
    pure a = Writer (a, ???)
    (Writer (f, w1)) <*> (Writer (a, w2)) = Writer (f a, ???)
```

Two problems here:
- In `pure`, how can we construct an object when we know nothing about its type?
- In `(<*>)`, how should we combine two `w`s? We surely cannot just dump one of them leaving only the other since the logging infomation would be lost!

Two problems rises naturally, since we are able to define program with some output, we should be able to define program with "empty" output, that's what we want here. And we should know some way to accumulate the output as we previously said.

So we gotta put some constraint on the type parameter `w` to know more about that, something like this:

``` haskell
class Log l where
    empty :: l
    accumulate :: l -> l -> l
```

Luckily, there's actually a typeclass in base library looks exactly like this, the `Monoid`

#### Some detour to `Monoid`

``` haskell
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a
```

Monoid is a term that comes from abstract algebra, which is a algebra structure with some properties defined. But as I promise, we don't care much about math here. We call type `a` a `Monoid` as long as:

- We can choose an "empty" value for type `a`
- We combine values in type `a` **associatively** (that is, `mappend` should have associativity)
- `mappend mempty a` === `mappend a mempty` === `a`

(Note that **no commutativity** of `mappend` is guaranteed)

The most common (maybe not) monoid in haskell maybe the List, it's defined like:

``` haskell
instance Monoid [a] where
    mempty = []
    mappend = (++)
```

Since `String` is `[Char]` in disguise, so `String` is also a monoid.

There's [an article in haskell wiki](https://wiki.haskell.org/Monoid) introducing `Monoid` if you want to know more.

#### Back to defining writer monad

Life becomes quite easy and happy now. Go back to where we leave.

``` haskell
instance Monoid w => Applicative (Writer w) where
    pure a = Writer (a, mempty)
    (Writer (f, w1)) <*> (Writer (a, w2)) = Writer (f a, mappend w1 w2)
```

And the monad

``` haskell
instance Monoid w => Monad (Writer w) where
    return = pure
    (Writer (a, w)) >>= f =
        let (Writer (b, w')) = f a in Writer (b, mappend w w')
```

Now convince yourself these give `Writer` the ability to generate and combine output, it's pretty obvious though.

### Usage

We're using writer because we want produce extra output in our program. So how do we do that?

In `mtl` this is performed by `tell`, which only produce output and a trivial value (the unit).

``` haskell
tell :: w -> Writer w ()
tell w = Writer ((), w)
```

Then comes a trivial little program that calculate the answer of universe while logging.

``` haskell
getAnswer :: LogWriter Int
getAnswer = do
    tell "generating answer of everything\n"
    let answer = againDoSomeSeriousCalculation
    tell $ printf "answer is %d!\n" answer
    return answer
```

And running the writer gets us:

``` haskell
𝝺 > runWriter getAnswer
(42,"generating answer of everything\nanswer is 42!\n")
```

Now we know how `Writer` work, let us (I mean you here) define some useful utilities for it.

``` haskell
tell :: Monad m => w -> Writer w ()
tell w = Writer ((), w)

-- listen adds the output of a writer to it's computation result
listen :: Writer w a -> Writer w (a, w)

-- listens maps the output before adding it to the result
listens :: (w -> b) -> Writer w a -> Writer w (a, b)

-- cencor executes the writer and apply a function to its output
cencor :: (w -> w) -> Writer w a -> Writer w a

-- pass transform the output according to the result of computation
pass :: Writer w (a, w -> w) -> Writer w a
```

## State

### Jump right in

Now we can read something, we can write something, you may wonder if there's a monad providing both reading and writing capability, and that's `State`. We can handle mutable states in our program in a purely functional manner with the help of `State`. Let's see how to do it.

`State`, in some sense, is a combination of `Reader` and `Writer`. Its definition also looks like so.

``` haskell
newtype State s a = State { runState :: s -> (a, s) }
```

The type signature indicates that a state object is a function that takes the original state and returns the computation result and the new state after the carrying out the computation.

Then let's define its monad instance.

``` haskell
instance Functor (State s) where
    fmap f (State sf) = State $ \s -> let (a, s') = sf s in (f a, s')
    -- or alternatively
    -- import Data.Bifunctor (first)
    -- fmap f (State sf) = State $ fmap (first f) sf
```

The definition consists with more letter, but the idea behind it is simple. `fmap` only maps the result of computation without changing anything. (If you wonder, the alternative definition of `fmap` makes use of the fact that function is a `Functor` and two-tuple is a `Bifunctor`, you may look them up yourself.)

Then we define the `Applicative`.

``` haskell
instance Applicative (State s) where
    pure a = State $ \s -> (a, s)

    (State sf) <*> (State sa) = State $ \s ->
        let (f, s') = sf s
            (a, s'') = sa s'
        in  (f a, s'')
```

`pure` simply makes a constant function that produce constant output and the input state is intact.

The state passing of `(<*>)` should be handle with care, the sequence matters here. We first runs the first state object obtaining the modified state, then we throw the modified state into the second state object obtaining the final state. We can do it in reverse order of course, but passing state from left to right by default is more intuitive. (There _are_ circumstances where defining state reversely makes more sense).

Finally is monad, it should be simple and familiar as you reach here.

``` haskell
instance Monad (State s) where
    return = pure

    (State fa) >>= f = State $ \s ->
        let (a, s') = fa s
            (State fb) = f a
        in  fb s' -- (b, s'')
```

In `(>>=)`, we first run the original state getting the result, and generate another state object yielding `b`, then pass the modified result to it.

### Usage

And since state support both reading and writing, we need them as primitives to deal with states more conveniently. We define two functions `get` and `put` to achieve that. Look at the sigature, see if you can implement them yourself before reading on.

``` haskell
get :: State s s

put :: s -> State s ()
```

`get` is similar to `ask` of `Reader`. It direct the input state to the computation result, so that we can extract it in the `do` block (`(>>=)`, actually). `put` set the output state directly ignoring the input state (why can this be useful? The answer should be very obvious).

So here it is:

``` haskell
get = State $ \s -> (s, s)

put s = State $ const ((), s)
```

Here's a factorial function illustrating the use of mutable state.

``` haskell
stateFact :: State (Int, Int) Int
stateFact = do
    (count, acc) <- get
    if count == 0
        then return acc
        else do
            put (count - 1, acc * count)
            stateFact

fact :: Int -> Int
fact i | i < 0 = error "negative factorial"
       | otherwise = fst $ runState stateFact (i, 1)
```

And finally, there's some other useful utility functions we can define on `State`.

``` haskell
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put = modify . const

-- modify state with function
modify :: (s -> s) -> State s ()

-- again, gets is a generalized version of `get`
gets :: (s -> a) -> State s a
```

### Why `Reader` and `Writer` then?

`State` is strictly more powerful than both `Reader` and `Writer`, why not use `State` all the time then? Indeed, yes, while situations where only functionalities of `Reader` or `Writer` are very prevalent, abstracting those situations out should be a wise choice, and when we are writing program, we always wants to use as little abstraction as possible, to promote both readibility and scalibility.

When looking at `Reader`, we are dead sure we **are not able** modify the environment in a way visible to other part of the program. And `State` is a whole another story, which stresses that the execution of program have a rather high chance to modify the state, "you better know what you are doing" when executing a state object.

### Why not `State` all the time then?

As we saw in the example above, we can almost simulate the common imperative language by the mutability `State` provide and the `do` syntax, why not do it all the time then?

You can, of course. But why you are choosing a purely functional language to do this?

## Conclusion

In this post we saw the power of monads that heavily make use of the idea of "context" on which our computation depends. "_computational effect_" decribes such phenomenon, when we are executing program (in this case, monad), we are not only getting the final computation result, but also affect the surrounding environment, and commonly-known "side effects" also falls in this category.

Anyway, we added several useful tools to our toolbox of functional programming, and we should have more confort doing read-world programming.

In next post, I will introduce another very powerful tool of monad, the Monad Transformer, which gaves us the ability to combine different monads and use it as one. And then, the basic monad introduction should come to an end.