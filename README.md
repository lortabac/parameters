# Parameters

Implicit parameters with `Reader`-like semantics.

Warning: experimental code, use at your own risk!

## The `parameters` library

This library provides an implementation of type-safe dynamic scoping in Haskell.
If you are familiar with `transformers`, you can think of it as a `Reader` that doesn't require a monad.

### Example

Let's start with an example.

Suppose you have an application that uses a database.
The connection handle is created at startup and is needed by various functions that are syntactically far from where it is created.

#### Explicit argument passing

One simple solution is to carry the handle around as an explicit argument.

```Haskell
main :: IO ()
main = do
  conn <- newDatabaseConnection
  startApp conn

startApp :: Connection -> IO ()
startApp conn = do
  initDatabase conn
  ...

initDatabase :: Connection -> IO ()
initDatabase conn = do
  runDBTasks conn
  ...
```

It works well but it has two drawbacks:
- It is verbose.
- It doesn't give you a way to distinguish between arguments that are meant to be consumed locally and configuration arguments that need to be available in a wide part of the code base.
  
  This is particularly relevant in complex applications where "configuration" parameters often outnumber local parameters. Let's consider for example an application that uses a database, emits logs and keeps metrics. If you pass all the arguments explicitly you may end up with lots of functions with signatures such as `Connection -> Logger -> MetricsBackend -> UserId -> IO ()` where `UserId` is the only "real" parameter and the rest is just "environment".

#### Implicit parameters

This library offers an alternative: it allows you to carry around some arguments *implicitly* as type-class constraints.

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
  ...

initDatabase :: HasParam ConnParam Connection => IO ()
initDatabase = do
  runDBTasks (ask @ConnParam)
  ...
```

Let's have a closer look at this code snippet:

- `runParam @ConnParam conn startApp` means "start the function `startApp` with the parameter `ConnParam` set to the value `conn`".
- `HasParam ConnParam Connection` is a constraint that means "a parameter called `ConnParam` of type `Connection` is in scope in this function".
- `ask @ConnParam` means "retrieve the parameter called `ConnParam`".

As you can see, the `ConnParam` parameter is available in `initDatabase` even though it is defined in a lexically separate location.
The compiler takes care of propagating the argument implicitly up to the place where it is used.

### The typechecker plugin

If you enable the `Param.Plugin` plugin, the compiler will check that `HasParam` can be solved unambiguously.
Trying to access a parameter that has more than one definition in scope will result in a type error.

```Haskell
{-# OPTIONS_GHC -fplugin Param.Plugin #-}

foo :: Int
foo = runParam @Foo 1 $ runParam @Foo 2 $ ask @Foo -- Doesn't compile
```

### Parameters as interfaces

Implicit parameters can be used to instantiate abstract interfaces with concrete implementations at runtime.
The idea is to define a record of functions for the interface:

```Haskell
-- | An interface for file reading and writing
data FileRW = FileRW
  { _readFile :: FilePath -> IO String
  , _writeFile :: FilePath -> String -> IO ()
  }
```

and an implicit parameter that holds the implementation:

```Haskell
-- | An implicit parameter that holds the 'FileRW' implementation
data FileRWParam
```

The implicit parameter is not exported. We offer a high-level API instead:

```Haskell
type HasFileRW = HasParam FileRWParam FileRW

runFileRW :: FileRW -> (HasFileRW => a) -> a
runFileRW = runParam @FileRWParam

readFile' :: HasFileRW => FilePath -> IO String
readFile' = _readFile (ask @FileRWParam)

writeFile' :: HasFileRW => FilePath -> String -> IO ()
writeFile' = _writeFile (ask @FileRWParam)
```

Then you can offer a default implementation that does the obvious thing:

```Haskell
defaultFileRW :: FileRW
defaultFileRW = FileRW
  { _readFile = System.IO.readFile
  , _writeFile = System.IO.writeFile
  }
```

If the 'FileRW' type is exported, users of this module can define their own alternative implementations:

```Haskell
mockFileRW :: FileRW
mockFileRW = FileRW
  { _readFile _ = pure "mock"
  , _writeFile _ _ = pure ()
  }
```

## The `parameters-fx` library

While the simplicity of this solution may be appropriate for lots of use cases,
in complex applications a more fine-grained definition of side effects can be beneficial to understand the behavior of the various components of the program.

That's where the `Fx` type becomes useful.

`Fx` is an alternative IO type.
It is a `Monad` like `IO` but it has an additional restriction:
every IO action that is performed in `Fx` must be part of an "effect",
that is a module where a bunch of actions are defined, together with an associated constraint.

### Effects that depend on implicit parameters

One way to embed an `IO` action into `Fx` is to attach it to an implicit parameter, via the `fx` function.

In the case of the "FileRW" effect, the parameter is `FileRWParam`:

```Haskell
readFile :: HasFileRW => FilePath -> Fx String
readFile path = fx @FileRWParam $ readFile' path

writeFile :: HasFileRW => FilePath -> String -> Fx ()
writeFile path contents = fx @FileRwParam $ writeFile' path contents
```

`fx`'s signature guarantees that each function that depends on an effectful action will need the constraint
that is associated to the corresponding effect.
In this case every function that depends on either `fxReadFile` or `fxWriteFile` will need the `HasFileRW` constraint.

Moreover, since `FileRWParam` is not exported, we know that all the "FileRW" actions must be defined in the `FileRW` module.
If a parameter is not exported, an effect cannot be extended outside of its module.

### Effects that do not depend on implicit parameters

You can also define effects that do not depend on a parameter.

For example we may want to define a "Concurrency" effect, whose main action simply calls `forkIO`.
No interface is needed, since we don't want to provide the ability to override the default implementation.

This can be easily achieved with `fxNoParam`, which is similar to `fx` but requires `HasEffect` instead of `HasParam`
and an arbitrary data type instead of a parameter.

```Haskell
module Concurrency (
    HasConcurrency
  , runConcurrency
  , forkFx
  ) where

data ConcurrencyEff

type HasConcurrency = HasEffect ConcurrencyEff

runConcurrency :: (HasConcurrency => Fx a) -> Fx a
runConcurrency = runEffect @ConcurrencyEff

forkFx :: HasConcurrency => Fx () -> Fx ThreadId
forkFx action = fxNoParam @ConcurrencyEff $ forkIO action
```

### Performing arbitrary IO

The third way to embed an `IO` action into `Fx` is `embedIO`.
This method requires the `HasIO` constraint.

### Running the Fx monad

There are two ways to start an `Fx` computation:

1. `runFx`, which starts an `Fx` action in which no arbitrary `IO` can be performed.
2. `runFxWithIO`, which starts an `Fx` action in which arbitrary `IO` can be embedded with `embedIO`.
