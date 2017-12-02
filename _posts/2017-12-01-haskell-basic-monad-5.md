---
layout: post
title: "Haskell Monad Basic - 5"
excerpt: Monad Transformer
category: [haskell monad basic]
modified: 2017-12-01
---

# Monad Transformer

## Motivation

As we saw in previous posts, each kind of monad has their specific purpose, `Maybe` and `Either` for error handling, `Writer` for recording and combining output, `Reader` for accessing global environment etc. But solving one problem may need several monad abstractions, for example we may need both error handling and state manipulation in a single program. How do we do that?

## Introduction

Consider a simple example, we running a program that needs both io operation and error handling, so we use `IO` hand `Maybe` here. We may want to write the type as follow.

``` haskell
type IOMaybe a = IO (Maybe a)
```

It's perfectly io with error handling huh. It works with no doubt, but problem of `Maybe`-without-monad arises again here when we want to chaining the actions, and probably even worse than we thought.

``` haskell
actionA :: IO (Maybe A)
actionB :: A -> IO (Maybe B)
actionC :: B -> IO (Maybe C)

chainAction :: IO (Maybe C)
chainAction = do
    ma <- actionA
    case ma of
        Just a -> do
            mb <- actionB a
            case mb of
                Just b  -> actionC b
                Nothing -> return Nothing
        Nothing -> return Nothing
```

We avoid pattern matching every time when we want to extract value from `Maybe` by making it a monad. But the monad instance of `Maybe` doesn't help us here, it only works for expressions that have `Maybe` as the monad of the result, but here the `IO` dominates the `do` block, and unfortunately `Maybe`s are the values inside of `IO`, so we can do nothing about that.

Or can we?

### IOMaybe as a monad

Look at the code again, we saw similar patterns over and over.

- We run an IO action, extract maybe object from it.
- We pattern match on the maybe object
- If it contains a value, we extract it, or just `return Nothing` otherwise

So all we need is a tool that abstract this process out! Yes, let's make it a monad.

``` haskell
newtype IOMaybe a = IOMaybe { runIOMaybe :: IO (Maybe a) }

instance Functor IOMaybe where
    fmap f = IOMaybe . fmap (fmap f) . runIOMaybe

instance Applicative IOMaybe where
    pure = IOMaybe . pure . pure
    (IOMaybe iomf) <*> (IOMaybe ioma) = IOMaybe $ liftA2 (<*>) iomf ioma

instance Monad IOMaybe where
    return = pure
    (IOMaybe iom) >>= f = IOMaybe $ do
        m <- iom
        case m of
            Just a  -> runIOMaybe $ f a
            Nothing -> return Nothing
```

Then we can use `IOMaybe` like this!

``` haskell
actionA :: IOMaybe A
actionB :: A -> IOMaybe B
actionC :: B -> IOMaybe C

chainAction :: IOMaybe C
chainAction = do
    a <- actionA
    b <- actionB a
    actionC b
```

Great! The pattern matching hell is completely gone!

### `MaybeT`, the Maybe Transformer

But wait, when you are looking at the monad definition above, does it have anything to do with `IO` at all? Err, I mean ... we may replace IO to any other monad, right? Let's do this.

``` haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
    fmap f = MaybeT . fmap (fmap f) . runMaybeT

instance Applicative m => Applicative (MaybeT m) where
    pure = MaybeT . pure . pure
    (MaybeT mf) <*> (MaybeT ma) = MaybeT $ liftA2 (<*>) mf ma

instance Monad m => Monad (MaybeT m) where
    return = pure
    (MaybeT m) >>= f = MaybeT $ do
        ma <- m
        case ma of
            Just a  -> runMaybeT $ f a
            Nothing -> return Nothing
```

Nothing really change here except for parameterizing the monad, then `IOMaybe` is just...

``` haskell
type IOMaybe = MaybeT IO
```

### the Monad Transformer

The `MaybeT` we define above is just one of the monad transformers. It's a convention to name a monad transformer of `SomeMonad` to `SomeMonadT`.

The maybe transformer transforms another monad to a maybe-like monad and embedding the behaviour **into** that monad. As we saw, we embeded the `Maybe` into the `IO` so that we can only extract value when there is any, and returning `Nothing` otherwise. As a result we add new functionality into the monad while preserving the original. We pick `MaybeT` as an example, but you now probably have some rought idea how other monad transformer works.

And notice that the "monad slot" of the `MaybeT` itself can be another monad transformer, i.e. the monad transformer is defined recursively, which enable us to merge more than two monads together.

## Monad Lifting

### Motivation

Honestly, when I said something like "add new **while preserving the original**", I lied, well, not completely though. We showed that `IOMaybe` can be used like a `Maybe`, where does `IO` goes then, how do we launch a normal IO action then?

Since something like this

``` haskell
chainAction :: IOMaybe C
chainAction = do
    a <- actionA
    putStrLn "actionA" -- Couldn't match type ‘IO’ with ‘MaybeT IO’
    b <- actionB a
    actionC b
```

doesn't work quite obviously, so we may want to fix that line with...

``` haskell
MaybeT $ Just <$> putStrLn "actionA complete"
```

But Every Single Time??

```haskell
chainAction :: IOMaybe C
chainAction = do
    a <- actionA
    MaybeT $ Just <$> putStrLn "actionA complete"
    b <- actionB a
    MaybeT $ Just <$> putStrLn "actionB complete"
    c <- actionC b
    MaybeT $ Just <$> putStrLn "actionC complete"
    return c
```

This is a serious problem.

### Lifting original monad to new context

Everytime we need to copy-&-paste something, we gotta abstract it out. The problem is how to choose an abstraction.

The problem here is that since we conceptually cover the `IO` with `MaybeT`, so plain IO action (monad) doesn't work here. So we can define a function for `MaybeT` to "wrap" the `MaybeT` around the inner monad.

``` haskell
liftMaybeT :: Monad m => m a -> MaybeT m a
liftMaybeT = MaybeT . fmap Just
```

then there're a lot less boilerplates

``` haskell
chainAction :: IOMaybe C
chainAction = do
    a <- actionA
    liftMaybeT $ putStrLn "actionA complete"
    b <- actionB a
    liftMaybeT $ putStrLn "actionB complete"
    c <- actionC b
    liftMaybeT $ putStrLn "actionC complete"
    return c
```

### `MonadTrans`

Actually, the "lifting" abstraction for `MaybeT` is also needed by other monad transformers, so why not provide a consistent interface for that, and that's `MonadTrans`.

``` haskell
class MonadTrans t where
    lift :: Monad m => m a -> t m a
```

`MonadTrans` only has a single method used for lifting inner monad (in terms of the type of transformer) to the transformer context. The lifting action should satify the following laws:

- Identity: `lift . return` === `return`
- Associativity: `lift (m >>= f)` === `lift m >>= (lift . f)`

which basically said the lifting operation shouldn't introducing new non-trivial context of the transformer.

Then we can define the `MaybeT` as an instance of `MonadTrans`

``` haskell
instance MonadTrans MaybeT where
    lift = MaybeT . fmap Just
```

At last we change all `liftMaybeT` to `lift` then we're done.

### `MonadIO`

If you looking at codes involving monad transformers and `IO`, you often see `liftIO` instead of the simple `lift`, so I think it's worth mentioning this here, although it's not completely necessary.

The motivation of `LiftIO` is that, since there is no transformer for `IO`, so that `IO` is always  appear as the innermost monad in the transformer stack, that is, the outermost monad after running the transformer. Being the bottom of the transformer stack, which means it usually need several times of lifting like...

``` haskell
type SomeBigTransformerStack = StateT S (ListT (MaybeT IO))

iJustWantToHelloWorld :: SomeBigTransformerStack ()
iJustWantToHelloWorld = lift $ lift $ lift $ putStrLn "hhhhello..?"
```

It ain't good.

So we may want to lift the inner most `IO` to the monad directly instead of lifting it multiple times. So we define another interface.

``` haskell
class Monad m => MonadIO m where
    liftIO :: IO a -> m a
```

which allow whatever monad (transformer, usually) being an instance of `MonadIO` to lift a plain IO operation to itself.

And of course, the base case, `IO` itself is a `MonadIO`

``` haskell
instance MonadIO IO where
    liftIO = id
```

As for monad transformers, if the monad it wraps can lift IO operation, it can. Take `MaybeT` as an example.

``` haskell
instance MonadIO m => MonadIO (MaybeT m) where
    liftIO = lift . liftIO
```

As a convention, all `MonadIO` instance is recursively defined for monad transformers. So the constraint deduction would converge to the `MonadIO IO` base case if there's a `IO` sitting at the bottom of the transformer stack.

Here's a cleaner io:

``` haskell
cleanerHelloWorld :: SomeBigTransformerStack ()
cleanerHelloWorld = liftIO $ putStrLn "hello world"
```

## `ReaderT`, `WriterT` and `StateT`

I said in the last post, the `Reader`, `Writer` and `State` in library aren't exactly like that. Actually, they are all specialization of moand transformers. Let's look at them closely.

``` haskell
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
```

Observing that ordinary `Reader`, `Writer` and `State` don't have the `m` part, so when we don't actually need a monad when a monad is required, we use the trivial monad `Identity`, (also trivial functor and applicative and many other thing), which is defined in `Data.Functor.Identity` like this:

``` haskell
newtype Identity a = Identity { runIdentity :: a }
```

Just as the type name indicates, an `Identity` of `a` is just `a` itself, except that it's wrapped in a parameterized type (with kind `* -> *`).

So the ordinary `Reader`, `Writer`, and `State`

``` haskell
type Reader r a = ReaderT r Identity a

runReader :: Reader r a -> r -> a
runReader m = runIdentity . runReaderT m

type Writer w a = WriterT w Identity a

runWriter :: Writer w a -> (a, w)
runWriter = runIdentity . runWriterT

type State s a = StateT s Identity a

runState :: State s a -> s -> (a, s)
runState m = runIdentity . runStateT m
```

Of course, all utility operations like `ask`, `tell`, `get`, `put` etc. should be altered to the monad transformer form. We would introduce them in the following section.

## the "Monadic Interface" in `MTL`

### Motivation

Lifting can be annoying, especially when there're multiple things to lift at multiple layer of the stack. Let's look at a trivial example.

Type signatures of `ask`, `tell` and `put` in haskell base library is defined as:

``` haskell
ask :: Monad m => ReaderT r m r

tell :: Monad m => w -> WriterT w m ()

put :: Monad m => s -> StateT s m ()
```

No surprise huh? Let's use them.

``` haskell
type RWS = ReaderT Int (WriterT String (State Char))

someAction :: RWS ()
someAction = do
    env <- ask
    lift $ tell "hello"
    lift $ lift $ put 'a'
```

Here .. if we use `Reader`, no lift is needed. For `Writer`, 1 lift. For `State`, 2 lifts. We know the number of lifting needed depends on the their position in the transformer stack. Although compiler always know how many lifts are needed in each case, it doesn't feels very good, especially when code gets bigger, and there're also other transformer stack with different order.

`liftIO` solve this problem for `IO` using the fact that `IO` is always in the innermost postition, as long as all the transformers above of it support `liftIO`, there's always one liftIO without thinking.

So how do we solve this problem for other monads?

(for combination of `Reader`, `Writer` and `State`, there's a monad named [`RWS`](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/transformers-0.5.2.0/Control-Monad-Trans-RWS-Lazy.html) combining and flattening the operation of all three monads, but it only works when you need exactly these three monads at the sametime, or you may provide trivial context (unit `()`, for example) for either of the three as a work around.)

### Monadic Interfaces

#### the Interface

To get rid of `lift`, we need utilities like `get` and `put` polymorphic, I mean, it can't simply returns concrete monad transformer like `StateT`, talking about polymorphism in haskell, how about making a typeclass?

``` haskell
class MonadState ??? where
    get :: ???
    put :: s -> ???
    state :: (s -> (a, s)) -> ???
```

We need to have access to the state type `s` in the class, but obviously the return value should be polymorphic. So we turn on the `{-# LANGUAGE MultiParamTypeClasses #-}` language feature to allow us to declare multiple type parameters in a single typeclass:

``` haskell
class Monad m => MonadState s m where
    get :: m s
    put :: s -> m ()
    state :: (s -> (a, s)) -> m a
```

Easy? Then we declare instances for the typeclass, of course we first want `StateT` to be `MonadState`. We should then turn on `{-# LANGUAGE FlexibleInstances #-}` to allow us to declare a partially applied parameterized type to be a instance of typeclass. If you don't know what I'm talking about, just ignore it.

``` haskell
instance Monad m => MonadState s (StateT s m) where
    get = StateT $ \s -> return (s, s)
    put s = StateT $ const (return ((), s))
```

#### the Functional Dependency

Great, let's test it first. Say we want to convert the integer state to a string.

``` haskell
intToString :: State Int String
intToString = do
    s <- get -- Error: Ambiguous type variable ‘a0’ arising from a use of ‘get’
    return $ show s
```

Wait, what? We declared the instance for `StateT` whose monad part (`m`) is `StateT s m`, and in this case `m` should be `StateT Int Identity String`, then `s` should perfectly be `Int`, what is ambiguous at all?

Let's look at the typeclass and instance definition again, `MonadState` itself **never** say: "if the second parameter is `StateT s m`, then the first parameter must be `s`, right? It's perfectly legal to declare a instance `MonadState String (StateT s m)` or something else, even there isn't one now. So the compiler cannot resolve `s` to `Int` according to currently defined instance with the receiver being a polymorphic function (`show`).

This problem is not trivial, and luckily we have another tool to solve it. We enable `{-# LANGUAGE FunctionalDependencies #-}`, and change the definition of `MonadState`:

``` haskell
class Monad m => MonadState s m | m -> s where
    get :: m s
    put :: s -> m ()
    state :: (s -> (a, s)) -> m a
```

The `m -> s` notation behind the bar has nothing to do with the function type. It declares the functional dependency over `m` to `s`, which means that for any type `m`, with which there would be at most one type `s` forms the instance of `MonadReader`. The functional dependency effectively promise to the compiler that we would only declare at most one `MonadState` instance for each `m`. Now the `intToString` above type checks, since compiler saw the `StateT` instance declaration, and knew `s` cannot be any other type but `Int`.

#### other Instances

To make `StateT` available for other monad transformers, we should declare other monad transformer. Borrowing the idea from instances of `MonadIO`, we want to say: if `s` and `m` are instance of `MonadState`, then `s` and `SomeOtherTransformer m` is `MonadState`, so when we are using `SomeOtherTransformer`, we can also use `get` and `put` to manipulate the state in the `State` it directly or indirectly wraps. I copy and paste some instances of `MonadState` here and you should have some feeling about how this works.

``` haskell
instance MonadState s m => MonadState s (MaybeT m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadState s m => MonadState s (ReaderT r m) where
    get = lift get
    put = lift . put
    state = lift . state

instance (Monoid w, MonadState s m) => MonadState s (WriterT w m) where
    get = lift get
    put = lift . put
    state = lift . state

-- more of them
```

Why not declare the instance like `instance (MonadTrans t, MonadState s m) => MonadState s (t m)` once and for all? The reason is that in this case, the `MonadTrans` is just enough for the `MonadState`, but it's not the general situations for monadic interfaces, so we'd better not treat the `MonadState` specially.

#### Other interfaces

Of course, there are corresponding interfaces for `Writer`, `Reader`, `Except` and `SomeOtherTransformer` naming `MonadWriter`, `MonadReader`, `MonadExcept` and `MonadSomeOtherTransformer`. And they're all compatible to every other transformers in the `MTL`.

It's a lot of work, of course. While since monad transformers are used a lot more often than created, the boilerplates are totally bearable, comparing to the complexity reduced of client code.

#### Finally

With all these effort, we finally get rid of appreciable amount of `lift`s. The sample in the beginning of this section can be improved to:

``` haskell
type RWS = ReaderT Int (WriterT String (State Char))

someAction :: RWS ()
someAction = do
    env <- ask   -- these
    tell "hello" -- all
    put 'a'      -- type checks
```

At this point you should convince yourself the trivial example above actually works. If you have any problem, you may consult the [documentation and api of mtl](https://hackage.haskell.org/package/mtl).

## Other Notes about Monad Transformers

### the `ExceptT`

A basic transformer we didn't introduce before is the `ExceptT`, whose underlying monad is `Either`. As we mentioned previously, the `Either` provide the abstraction of "error handling", `ExceptT` and `MonadExcept` provide more useful utilities of that, we can `throwError` and `catchError`, and have a feeling of doing imperative programming. But there's nothing really complicated behind it, so I guess it's ok for you (to implement that?).

### the stacking order matters

The stacking order of monad transformer affects the final result of program, for example

``` haskell
ListT Maybe a === Maybe [a]
MaybeT [] a === [Maybe a]

StateT s (Writer w) a === (s -> (s, a), w)
WriterT w (State s) a === s -> ((a, w), s)

-- etc.
```

So you should carefully choose correct monads and stack them in the correct order in terms of the meaning of your program. Especially when error handling is involved.

### user defined transformers

On the user perspective, if you insist on defining a new monad and used with other monad transformer in the `mtl` library. You can simply place your monad at the bottom of the transformer stack without without extending the `mtl` library.

And if you also your own monadic interface, and the number of instances you define depend on the purpose of your code (only a part of your program, or as a transformer library).

### Import paths

The monad transformers in haskell base library are under `Control.Monad.Trans.XXX`, like `Control.Monad.Trans.State`, and for some monads the library provide both strict and lazy version (the strictness and laziness is in terms of pattern matching), the lazy version is used by default, if you want to use the strict version, you should import something like `Control.Monad.State.Strict`

For `mtl`, they are `Control.Monad.XXX` like `Control.Monad.State`, also, `mtl` makes use of the strict and lazy versions of transformer in the base library, so it also provide `Control.Monad.XXX.Strict` and `Control.Monad.XXX.Lazy` if there are ones in the base library.

## Conclusion & What to do next?

In this post I covered some basic concept and implementation of the monad transformer, I hope those makes sense to you. In real-world, the reason way we seldom create monad ourselves is because of the flexibility of monad transformers mtl provide, we stack different monads with different order to encapusulate different semantic to our program. Such as an interpreter often includes the `State` and `Except`, command line tools often something sombining `IO` and `Reader`, server running constantly is something `State` and `Writer` etc.

The five posts in the series introduce almost everything of monad that I think is important for beginner of haskell and functional programming. And hopefully the word `monad` doesn't scare you any more.

Functional programming, haskell and monad are all extremely complex things, what I know and understand is only the tiniest bit among them. There're more commonly-used monads I didn't introduce here like `Parser` and `Cont` (Continuation), and a lot more about the monad typeclass family and so on. So the end of the series doesn't mean an end at any degree.

Good luck from now.