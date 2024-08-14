# Parameters

Implicit parameters with sensible semantics.

## Description

This library provides an implementation of type-safe dynamic scoping in Haskell.
If you are familiar with `transformers`, you can think of it as a `Reader` that doesn't require a monad.

Let's start with an example.

Suppose you have an application that uses a database.
Some functions in your application will need the database connection handle. Some won't.

One simple solution is to carry the handle around as an explicit argument.
It works well but it has two drawbacks:
- It is verbose.
- It doesn't give you a way to distinguish between arguments that are meant to be consumed locally and configuration arguments that need to be available in a wide part of the code base.

This library offers an alternative: it allows you to carry around the argument *implicitly* as a type-class constraint.

```Haskell
import Param

data ConnParam

main :: IO ()
main = do
  conn <- newDatabaseConnection
  runParam @ConnParam conn startApp

startApp :: HasParam ConnParam Connection => IO ()
startApp = do
  initDatabase
  startHttpServer
  startCronJobs

initDatabase :: HasParam ConnParam Connection => IO ()
initDatabase = runDBTasks (ask @ConnParam)
```

Let's have a closer look at this code snippet:

- `runParam @ConnParam conn startApp` means "start the function `startApp` with the parameter `ConnParam` set to the value `conn`".
- `HasParam ConnParam Connection` is a constraint that means "a parameter called `ConnParam` of type `Connection` is in scope in this function".
- `ask @ConnParam` means "retrieve the parameter called `ConnParam`".

As you can see, the `ConnParam` parameter is available in `initDatabase` even though it is defined in a lexically separate location.
The compiler takes care of propagating the argument implicitly up to the place where it is used.

## The typechecker plugin

If you enable the `Param.Plugin` plugin, the compiler will check that `HasParam` can be solved unambiguously.
Trying to access a parameter that has more than one definition in scope will result in a type error.

```Haskell
{-# OPTIONS_GHC -fplugin Param.Plugin #-}

foo :: Int
foo = runParam @Foo 1 $ runParam @Foo 2 $ ask @Foo -- Doesn't compile
```
