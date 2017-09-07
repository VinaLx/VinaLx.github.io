---
layout: post
title: "[draft] Haskell Monad Basic - 1"
excerpt: introduction, the "evolution" of programming pattern, basic usage and the "do" notation
modified: 2017-09-07
---

This series of articles would be basically my personal study reviews.
I would present an overview about monad, and hopefully make it less dreadful as its name looks like for new haskell or functional programming learners.

I assume readers to have a basic grasp of haskell syntax and a liiiittle experience in programming in functional style, but no math background would be required.

# Haskell Monad Basic 1

`Monad` is maybe the most important programming pattern in haskell, the name "Monad" came from mathematic, but it (maybe) doesn't really matter that a programmer doesn't understand what its exact mathematical definition is, so do many other names we came across in haskell.

Monad exists everywhere in haskell, we met it when we typed the first line of haskell program

~~~ haskell
main :: IO ()
main = putStrLn "Hello World"
~~~

OK there're two lines. Usually textbook would say we use `IO a` whenever we want to interact with the outside world. But that's not the whole story, IO itself is an instance of the mysterious Monad typeclass, which enable us to write program like

~~~ haskell
main :: IO ()
main = do
    putStrLn "what's your name?"
    name <- readLn
    putStrLn "hello " ++ name
~~~
