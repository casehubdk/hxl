# Delimited Errors for Batched Queries

## Abstract

This note presents a small query language for batched data fetching with local
typed error channels. Requests are represented as free applicative expressions.
Programs are represented as bind trees whose leaves are request batches or
raised domain errors. An evaluator repeatedly finds the next batch, runs it,
and resumes the suspended continuations.

The same syntax also supports delimited error handling. A channel introduces a
scoped raising capability. The handler is a tree transformation over the query
syntax, not an exception mechanism in the base effect. Placing the delimiter
inside or outside a traversal changes the error scope while preserving the same
batching structure where the same requests are visible.

```haskell
traverse (\x -> channel (\raise -> f x raise)) xs
-- Hxl f [Either e a]

channel (\raise -> traverse (\x -> f x raise) xs)
-- Hxl f (Either e [a])
```

## Motivation

Batched query libraries let a program describe one request at a time while an
interpreter groups independent requests. The programmer usually writes ordinary
monadic code:

```haskell
program x = do
  user <- getUser x
  org  <- getOrg (orgId user)
  pure (user, org)
```

Such programs are commonly traversed over a collection:

```haskell
traverse program xs
```

The interpreter should then batch the visible requests, for example by turning
several calls to `getUser` into one call to the corresponding data source.

Conditionals matter because they change the shape of the computation for each
element:

```haskell
program x = do
  a <- getA x
  b <- if useB a
          then Just <$> getB a
          else pure Nothing
  c <- getC a
  pure (a, b, c)
```

For one element the middle branch may fetch `b`; for another it may be pure and
continue to `c`. The program is written in do notation, and do notation denotes
a bind tree. A scheduler that reduces each element to a linear spine loses
structure that is useful for later alignment.

Errors introduce a second axis of structure. A caller may want per-element
handling:

```haskell
traverse (\x -> channel (\r -> f x r)) xs
```

or one channel around the whole traversal:

```haskell
channel (\r -> traverse (\x -> f x r) xs)
```

Batching and error handling historically mix poorly.
However this work will show that both problems can be solved succinctly.

## Data Sources and Requests

A data source batches keys of one type and returns values of one type:

```haskell
data DataSource f k a = DataSource
  { fetchMany :: NonEmpty k -> f (Map k a)
  }
```

A primitive request is an existential pair of a data source and one key:

```haskell
data Request f a where
  Request :: Ord k => DataSource f k a -> k -> Request f (Maybe a)
```

The `Ord k` constraint is representative of the usual need to re-associate
results with keys. A concrete implementation also needs a way to identify data
sources for grouping. That detail is orthogonal to the calculus.

A request batch is a free applicative over primitive requests:

```haskell
type Requests f = Ap (Request f)
```

Thus `Requests f a` is an applicative expression whose leaves are individual
requests. The interpreter can collect the leaves, group them by data source,
fetch each group, and rebuild the final `a` using the applicative structure.

## Query Syntax

The query language separates three forms:

```haskell
data Hxl f a where
  Run  :: Requests f a -> Hxl f a
  Bind :: Hxl f b -> (b -> Hxl f a) -> Hxl f a
  Errs :: [Raised] -> Hxl f a
```

`Run` contains the next batchable request expression. `Bind` represents dependent
continuation. `Errs` contains raised domain errors that have not yet been
handled.

Pure values are pure request batches:

```haskell
pureHxl :: a -> Hxl f a
pureHxl = Run . pure
```

The monadic operation records a bind:

```haskell
bindHxl :: Hxl f a -> (a -> Hxl f b) -> Hxl f b
bindHxl = Bind
```

The standard instances use these definitions.

The main query type has no error parameter:

```haskell
Hxl f a
```

Typed errors are introduced by channels, described below.

## Applicative Composition

The applicative instance is the part of the calculus that exposes later
batches without discarding monadic structure. The following is close to the
whole definition; production code still needs the usual stack-safety and effect
translation cases.

```haskell
instance Applicative (Hxl f) where
  pure = Run . pure
  (<*>) = apHxl

apHxl :: Hxl f (a -> b) -> Hxl f a -> Hxl f b
apHxl ff xx =
  case (ff, xx) of
    (Errs e1, Errs e2) -> Errs (e1 <> e2)
    (Errs e, _) -> Errs e
    (_, Errs e) -> Errs e

    (Run rf, Run rx) -> Run (rf <*> rx)

    (Bind q k, Bind p h) ->
      Bind (pair q p) $ \(x, y) ->
        apHxl (k x) (h y)

    (Bind q k, y) ->
      Bind (pair q y) $ \(x, a) ->
        apHxl (k x) (pure a)

    (x, Bind q k) ->
      Bind (pair x q) $ \(f, y) ->
        apHxl (pure f) (k y)

pair :: Hxl f a -> Hxl f b -> Hxl f (a, b)
pair q p = (,) <$> q <*> p
```

The `Run` case combines independent request batches. The `Errs` cases give
applicative accumulation for domain errors. The `Bind` cases align the heads of
two bind trees and combine their continuations recursively.

## Bind Trees, Not Spines

When two traversed elements have the same dependency shape, a linear picture is
adequate:

```text
left:        a -> b -> c
right:       a -> b -> c

merged:      a -> b -> c
```

Conditionals break this picture:

```text
left:        a -> b -> c
right:       a      -> c

bad:         a -> (b, c) -> c
```

The right-side `c` has been aligned with the left-side `b`. The two `c`
requests are no longer in the same position.

The bind-tree representation keeps the missing branch as a hole:

```text
left:        ((a, b), c)
right:       ((a, ()), c)

merged:      ((a, b), c)
```

The `()` is only a diagrammatic hole: the right program had no request
corresponding to `b`. It is not an assertion that `pure ()` has the same
meaning as `b`. The important point is that the right-side `c` remains after
the same outer bind, so it can align with the left-side `c`.

This is exactly the structure produced by do notation and then combined by
`traverse`. Each element contributes a bind tree. The applicative instance
merges those trees and recurses into continuations, so the same argument applies
below later binds.

## Batch Stepping

The evaluator repeatedly finds the next batch:

```haskell
data Step f a where
  Done    :: a -> Step f a
  Blocked :: Requests f b -> (b -> Hxl f a) -> Step f a
  Failed  :: [Raised] -> Step f a
```

The stepper evaluates through pure request batches and binds until it reaches
either an unresolved request batch or unhandled errors:

```haskell
step :: Hxl f a -> Step f a
step query =
  case query of
    Errs es -> Failed es
    Run r ->
      case doneRequests r of
        Just a  -> Done a
        Nothing -> Blocked r pure
    Bind q k ->
      case step q of
        Done a -> step (k a)
        Failed es -> Failed es
        Blocked r resume ->
          Blocked r (\x -> Bind (resume x) k)
```

Here `doneRequests` recognizes request batches with no request leaves. The
remaining interpreter loop is direct:

```haskell
run :: Monad f => Hxl f a -> f a
run q =
  case step q of
    Done a      -> pure a
    Failed es   -> unhandled es
    Blocked r k -> runRequests r >>= run . k
```

This is an operational account: find a batch, run it, resume.

## Error Channels

Errors are also syntax. A raised error stores a scoped tag and a value:

```haskell
data Tag s e

data Raised where
  Raised :: Tag s e -> e -> Raised
```

A channel introduces a fresh tag and a raising capability:

```haskell
newtype Raise s f e = Raise
  { raise :: forall a. e -> Hxl f a
  }

channel
  :: Semigroup e
  => (forall s. Raise s f e -> Hxl f a)
  -> Hxl f (Either e a)
```

The token `s` scopes the capability in the same style as `runST`. Since `a` is
chosen outside `forall s`, the result cannot mention `s`; the capability cannot
escape from `channel`.

Semantically, `channel` builds a handler for the tag it introduced:

```haskell
channel
  :: Semigroup e
  => (forall s. Raise s f e -> Hxl f a)
  -> Hxl f (Either e a)
channel body =
  freshTag $ \tag ->
    handle tag (body (Raise (\e -> Errs [Raised tag e])))
```

The handler is the semantically relevant operation:

```haskell
handle :: Semigroup e => Tag s e -> Hxl f a -> Hxl f (Either e a)
handle tag query =
  case query of
    Run r -> Run (Right <$> r)

    Errs es ->
      case split tag es of
        ([], foreign) -> Errs foreign
        (local, _)    -> pure (Left (sconcat local))

    Bind q k ->
      Bind (handle tag q) $ \case
        Left e  -> pure (Left e)
        Right a -> handle tag (k a)
```

`Run` is preserved. Local errors become `Left`. Foreign errors remain
unhandled. `Bind` is traversed recursively, so inner handlers interpret their
own errors before an outer handler observes the surrounding program.

In particular:

```haskell
channel (\r -> raise r e) = pure (Left e)
```

## Delimiter Placement

Assume:

```haskell
f :: forall s. x -> Raise s m e -> Hxl m a
```

The caller chooses the error scope by placing the delimiter.

Per-element handling:

```haskell
traverse (\x -> channel (\r -> f x r)) xs
-- Hxl m [Either e a]
```

Whole-traversal handling:

```haskell
channel (\r -> traverse (\x -> f x r) xs)
-- Hxl m (Either e [a])
```

The first program gives each element its own channel. The second program gives
the traversal one shared channel. The query type remains `Hxl m a` inside
`f`; the error type appears only at the delimiter.

The delimiter changes which raises are handled together. Request grouping is
still determined by the query syntax.

## Relation to Haxl

Haxl introduced the central idea that monadic data-fetching programs can be
executed by collecting blocked requests, running them, and resuming suspended
continuations. The representation here keeps the request batch as syntax and
also represents domain errors as syntax. Error handling is therefore a
post-processing pass over the query value.

There is also a Lean formalization of the small calculus. Its role is to state
the syntax and channel equations independently of implementation details.

## Remaining Proof Work

A complete formal treatment should state:

1. the syntax of request batches;
2. the applicative merge relation for `Hxl`;
3. the batch stepping relation;
4. soundness of batch extraction;
5. the channel equations, including inner-channel precedence;
6. preservation of request structure by error handling.

The intended presentation should stay small: one query syntax, one batch
stepper, and one delimited handler for domain errors.

## References

- Simon Marlow et al., "There is No Fork: an Abstraction for Efficient,
  Concurrent, and Concise Data Access":
  <https://simonmar.github.io/bib/papers/haxl-icfp14.pdf>
- Haxl API documentation:
  <https://hackage.haskell.org/package/haxl-0.1.0.0/docs/Haxl-Core-Monad.html>
