# Delimited Errors for Batched Queries

## Abstract

Batching libraries let programs describe one request at a time while an
interpreter groups independent requests. Error handling introduces a separate
question: should an error fail one element of a traversal, the traversal as a
whole, or a local sub-computation? This note presents a small query language in
which requests and domain errors are both represented as syntax until
interpretation. Batching is obtained by normalizing to the next request
frontier. Error handling is expressed as a delimited pass over tagged error
nodes.

A representative example is:

```haskell
traverse (\x -> channel (\raise -> f x raise)) xs
-- Hxl f [Either e a]

channel (\raise -> traverse (\x -> f x raise) xs)
-- Hxl f (Either e [a])
```

The function `f` is the same in both programs. The delimiter determines the
error scope.

## Queries and Batches

A query program is naturally written in monadic style:

```haskell
program x = do
  user <- getUser x
  org  <- getOrg (orgId user)
  pure (user, org)
```

Such programs are often then traversed over a collection:

```haskell
traverse program xs
```

while the interpreter may want to find independent requests:

```haskell
getUsers [u1, u2, u3]
```

Conditionals make this less direct:

```haskell
program x = do
  a <- getA x
  b <- if useB a
          then Just <$> getB a
          else pure Nothing
  c <- getC a
  pure (a, b, c)
```

For one element, the conditional branch may fetch `b`; for another, it may be
pure and continue to `c`. The surface syntax is do notation, but the structure
is a bind tree. A scheduler that treats each element as a linear spine will miss
some request frontiers.

Errors add another dimension. The programmer may want:

```haskell
traverse (\x -> catchOne (f x)) xs
```

or:

```haskell
catchAll (traverse f xs)
```

These are different programs. The question is whether the query abstraction can
express both directly without giving up batching.

## Requests and Data Sources

A data source is a batch function for one kind of key:

```haskell
data DataSource f k a = DataSource
  { fetchMany :: NonEmpty k -> f (Map k a)
  }
```

An individual request pairs a data source with one key. The key type is
existential, since a request set may contain requests to several data sources:

```haskell
data Request f a where
  Request :: DataSource f k a -> k -> Request f (Maybe a)
```

A batch frontier is a free applicative over these primitive requests:

```haskell
type Requests f = Ap (Request f)
```

Thus `Requests f a` is an applicative expression whose leaves are individual
requests. Its leaves can be grouped by data source and fetched in batches, while
the applicative structure describes how to rebuild the final `a` from the
individual results.

## A Tiny Language

Ignoring implementation details, the core language can be presented as:

```haskell
data Hxl f a
  = Pure a
  | Fetch (Requests f a)
  | forall b. Bind (Hxl f b) (b -> Hxl f a)
  | Errors [Raised]
```

This is Haskell notation for the idea, not a complete concrete definition.
`Requests f a` is a batchable frontier which, once run, produces an `a`.
`Errors` contains typed errors hidden behind tags. The real Scala artifact uses
`Run Requests`, `AndThen`, and `Errs`; the smaller presentation is enough for
the argument here.

The monadic interface is ordinary:

```haskell
(>>=) :: Hxl f a -> (a -> Hxl f b) -> Hxl f b
(>>=) = Bind
```

The applicative interface combines independent frontiers:

```haskell
Fetch rf <*> Fetch rx = Fetch (rf <*> rx)
```

and domain errors accumulate applicatively:

```haskell
Errors e1 <*> Errors e2 = Errors (e1 <> e2)
```

## Applicative Composition

The applicative instance is the part of the calculus that exposes later
frontiers without discarding monadic structure. At this level of presentation,
it is a case analysis over visible constructors:

```haskell
Pure f <*> Pure x =
  Pure (f x)

Fetch rf <*> Fetch rx =
  Fetch (rf <*> rx)

Errors e1 <*> Errors e2 =
  Errors (e1 <> e2)

Errors e <*> _ =
  Errors e

_ <*> Errors e =
  Errors e
```

The cases involving `Bind` account for alignment. When both sides are monadic,
their heads are aligned and their continuations are combined afterwards:

```haskell
Bind q k <*> Bind p h =
  Bind ((,) <$> q <*> p) $ \(x, y) ->
    k x <*> h y
```

When only one side is monadic, the other side is moved into the head. This is
what lets a pure or already-visible branch advance to its next frontier while
retaining the continuation on the other side:

```haskell
Bind q k <*> y =
  Bind ((,) <$> q <*> y) $ \(x, a) ->
    k x <*> Pure a

x <*> Bind q k =
  Bind ((,) <$> x <*> q) $ \(f, y) ->
    Pure f <*> k y
```

These equations are schematic. Additional engineering cases are omitted here.

## Spines and Trees

One way to describe the batching problem is as a merge of dependency spines.
This is the shape produced when a do-program is traversed over a list: each
element contributes one bind tree, and those trees must be combined
applicatively.

When two programs have the same shape, the merge is straightforward:

```text
left:        a -> b -> c
right:       a -> b -> c

merged:      a -> b -> c
```

This picture is too simple for ordinary programs. A conditional may remove a
request from one branch while leaving later requests in place:

```text
left:        a -> b -> c
right:       a -> c
```

A spine-only merge tends to shift the second program:

```text
bad:         a -> (b, c) -> c
```

Here the right-side `c` has been aligned with `b` because the representation
has only a linear spine to work with. The useful structure is not a list of
requests, but a bind tree with holes:

```text
left:        ((a, b), c)
right:       ((a, ()), c)

merged:      ((a, b), c)
```

The `()` marks that the right program had no request corresponding to `b`.
Keeping the hole preserves the relative position of the right-side `c`, so it
aligns with the left-side `c` instead of being pulled into the earlier bind
merely because the right program was shorter at that point.

This is the role of the `Bind` applicative cases above. They merge the heads of
two bind trees and recursively combine the continuations. The same argument
applies at the next level, since each continuation again produces an `Hxl`
program.

Adding holes to a linear spine is not enough. For example:

```text
left:        a -> (b -> c) -> d
right:       a -> ()       -> d
```

The hole accounts for the missing middle segment as a whole, but the segment
`b -> c` is still opaque to the merge. There is no recursive structure in the
spine where later alignment can take place. In tree form the same shape retains
that structure:

```text
left:        ((a, (b, c)), d)
right:       ((a, ()), d)

merged:      ((a, (b, c)), d)
```

The hole must live in the bind tree, not as a placeholder in a flattened spine.

## Frontier Evaluation

The interpreter repeatedly exposes the next frontier:

```haskell
step :: Hxl f a -> Step f a

data Step f a
  = Done a
  | forall b. Blocked (Requests f b) (b -> Hxl f a)
  | Failed [Raised]
```

The relevant rule is ordinary monadic normalization:

```haskell
step (Bind (Pure a) k) = step (k a)
```

Pure branches can move forward until they reach their next request. Blocked
branches retain their continuations. Applicative composition combines visible
blocked frontiers:

```haskell
Blocked r1 k1 <*> Blocked r2 k2
  = Blocked ((,) <$> r1 <*> r2) (\(x, y) -> k1 x <*> k2 y)
```

Running a query is then a loop:

```haskell
run q =
  case step q of
    Done a        -> pure a
    Failed es     -> unhandled es
    Blocked r k   -> runRequests r >>= run . k
```

This is an operational account rather than a denotational model: evaluate far
enough to find the next batch, run that batch, then resume.

## Error Channels

Errors are also syntax. A raised error carries a fresh tag:

```haskell
data Tag e
data Raised = forall e. Raised (Tag e) e
```

A channel allocates a tag and passes a capability into the program:

```haskell
data Raise f e = Raise
  { raise :: forall a. e -> Hxl f a
  }

channel :: Semigroup e => (Raise f e -> Hxl f a) -> Hxl f (Either e a)
```

Conceptually:

```haskell
channel body =
  let tag = freshTag
      r   = Raise (\e -> Errors [Raised tag e])
  in handle tag (body r)
```

The handler is a tree transformation:

```haskell
handle tag (Pure a) =
  Pure (Right a)

handle tag (Errors es) =
  case select tag es of
    [] -> Errors es
    xs -> Pure (Left (sconcat xs))

handle tag (Fetch r) =
  Fetch (fmap Right r)

handle tag (Bind q k) =
  Bind (handle tag q) $ \case
    Left e  -> Pure (Left e)
    Right a -> handle tag (k a)
```

The operation `raise` does not throw and does not fail the underlying effect
`f`. It creates an error node in the query syntax. `channel` interprets that
node.

The tag identifies the capability. A channel handles errors raised through that
capability. Nested channels therefore delimit scopes. In Hxl's intended
semantics, an inner channel interprets errors from its own scope before the
surrounding program observes the result.

## Delimiter Placement

Suppose:

```haskell
f :: x -> Raise io e -> Hxl io a
```

The caller chooses the error scope by placing the delimiter.

Per-element handling:

```haskell
traverse (\x -> channel (\raise -> f x raise)) xs
-- Hxl io [Either e a]
```

Whole-traversal handling:

```haskell
channel (\raise -> traverse (\x -> f x raise) xs)
-- Hxl io (Either e [a])
```

The first program gives every element a private channel. One bad element
becomes one `Left`. Other elements may still succeed.

The second program gives the traversal one shared channel. Errors from many
elements accumulate into one `e`, and success requires the whole traversal to
succeed.

No separate data source API is involved. The query type does not carry a global
error parameter. Dependent monadic code and applicative error accumulation use
the same syntax tree.

## Basic Equations

The basic equations are small.

A channel catches its own raises:

```haskell
channel (\r -> raise r e) = pure (Left e)
```

A channel wraps successful values:

```haskell
channel (\_ -> pure a) = pure (Right a)
```

Applicative raises accumulate:

```haskell
channel (\r -> (,) <$> raise r e1 <*> raise r e2)
  = pure (Left (e1 <> e2))
```

Delimiter placement changes meaning:

```haskell
traverse (\x -> channel (\r -> f x r)) xs
  :: Hxl f [Either e a]

channel (\r -> traverse (\x -> f x r) xs)
  :: Hxl f (Either e [a])
```

Frontier batching is independent of that choice when the same requests are
visible:

```haskell
frontier (traverse (\x -> channel (\r -> f x r)) xs)
  =
frontier (channel (\r -> traverse (\x -> f x r) xs))
```

for the initial frontier of examples where `f` fetches before raising. More
generally, handling errors changes the error scope, while frontier discovery is
still governed by the query syntax.

## Why Not Just `EitherT` or `Validated`?

`EitherT f e a` gives typed monadic errors, but `e` is part of the whole
program's type. Local scopes must be introduced and discharged explicitly, and
the error axis tends to spread.

`Validated e a` accumulates errors well, but it is not the right interface for
dependent monadic queries. Once later requests depend on earlier results, the
program wants a monad again.

Here the main query type is still:

```haskell
Hxl f a
```

The error type appears only where a channel is opened:

```haskell
channel :: Semigroup e => (Raise f e -> Hxl f a) -> Hxl f (Either e a)
```

Thus typed errors are local capabilities rather than a global parameter of the
query type.

## Relation to Haxl

Haxl is the direct ancestor. It showed that monadic data-fetching programs can
be executed by collecting blocked requests, running them, and resuming. Hxl uses
a representation in which the request frontier and domain errors are both query
syntax, which makes post-processing available as an ordinary transformation.

The implementation may still use local mutation for efficient grouping. The
semantic point is that the error handler does not depend on an ambient exception
mechanism. It is a transformation of a query value.

## Relation to Fetch and ZQuery

Fetch and ZQuery also address batched queries. ZQuery additionally has a typed
error channel in the surrounding ZIO type. Hxl explores a different point: the
base query type has no error parameter, and typed domain errors are introduced
by delimited capabilities.

The relevant distinction is:

```text
Requests and domain errors can both be reified.
Batching is frontier evaluation.
Typed error handling is delimited post-processing.
The two compose.
```

## Artifact

The current artifact is the Scala library Hxl. The Scala implementation uses:

```scala
Done(value)
Run(requests)
AndThen(fa, fb)
Errs(raiseds)
```

The test suite includes the example:

```scala
xs.traverse(x => Hxl.channel(r => f(x, r)))
Hxl.channel(r => xs.traverse(x => f(x, r)))
```

For a function that fetches keys and raises on odd values, both programs fetch
all keys in one batch. The first returns per-item `Either`s. The second returns
a single accumulated error.

## What Remains

To turn this into a complete presentation:

1. Make the tiny Haskell calculus precise, probably with GADTs.
2. State the `step` relation formally.
3. Prove frontier soundness.
4. Prove the basic channel equations.
5. Keep the implementation section short and artifact-oriented.

The presentation should remain small: one query syntax, one frontier evaluator,
and one delimited handler for domain errors.

## References

- Simon Marlow et al., "There is No Fork: an Abstraction for Efficient,
  Concurrent, and Concise Data Access":
  <https://simonmar.github.io/bib/papers/haxl-icfp14.pdf>
- Haxl API documentation:
  <https://hackage.haskell.org/package/haxl-0.1.0.0/docs/Haxl-Core-Monad.html>
- Fetch documentation:
  <https://www.scala-exercises.org/fetch/usage>
- ZIO Query documentation:
  <https://zio.dev/zio-query/>
