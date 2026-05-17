# A Small Algebra for Batched Conditional Queries

## Abstract

Programs that read from services are pleasant to write as ordinary monadic
programs. A webshop quote, for example, loads a customer, optionally computes a
discount from the customer's coupon, and assembles a quote. The programmer sees
one order at a time.

The interpreter sees a different problem. It wants to batch all visible
requests: customers with customers, coupons with coupons, coupon discounts with
coupon discounts. Optional work makes this hard. A customer with a coupon has a
nested discount program; a customer without a coupon has a hole in the same
place. If the computation is treated as a flat sequence of phases, later
requests can be shifted into the wrong position.

This note presents a small algebra for batched conditional programs in the
Haxl style. The syntax has request frontiers, dependent binds, and raised domain
errors. Applicative composition is the merge operation for this syntax: it
combines visible frontiers and follows existing binds. Holes are preserved where
a branch did no work, so later requests keep their relative position. Error
delimiters use the same syntax to choose between partial completion and
whole-batch failure.

## The Checkout Program

Consider a quote for a webshop order:

```haskell
getDiscount product couponId = do
  coupon <- getCoupon couponId
  queryCouponDiscount coupon product

getQuote productId customerId = do
  customer <- getCustomer customerId
  discount <- traverse (getDiscount productId) customer.coupon
  assembleQuote customer productId discount
```

There are two paths. If the customer has a coupon, the batchable work has this
shape:

```text
p_1 = getCustomer -> (getCoupon -> queryCouponDiscount) -> assembleQuote
```

If the customer has no coupon, the nested discount program does no work:

```text
p_2 = getCustomer -> () -> assembleQuote
```

It is tempting to erase the unit and write the second program as:

```text
p_2' = getCustomer -> assembleQuote
```

That loses information. The absence of discount work is not the same as the
absence of a position in the computation. It is a hole inside the same branch
where the coupon program would have run.

## Spines and Trees

A scheduler for monadic programs cannot inspect past the next dependent
request. Once a request has returned, the continuation may reveal more work.
This is the reason to run all currently visible independent requests together,
resume their continuations, and repeat.

If each program is flattened into a spine of request phases, the two checkout
paths above appear to have different lengths:

```text
p_1 = getCustomer -> (getCoupon -> (queryCouponDiscount -> assembleQuote))
p_2' = getCustomer -> assembleQuote
```

Zipping such spines gives a bad alignment:

```text
p_1 +_spine p_2'
  = {getCustomer}
 -> {getCoupon, assembleQuote}
 -> {queryCouponDiscount}
 -> {assembleQuote}
```

The second quote's `assembleQuote` has moved into the coupon phase of the first
quote. The problem is not the batching of ready requests; it is the loss of the
program shape that says where the skipped work was skipped.

Even if the skipped branch is written explicitly, flattening still loses the
information that the unit belongs inside the discount branch:

```text
p_2 = getCustomer -> (() -> assembleQuote)
```

The algebra keeps the parentheses. The programs are trees rather than spines:

```text
p_1:

                              >>= (a)
                             /   \
              {getCustomer_1}     >>= (b)
                                  /   \
                              >>= (c)  {assembleQuote_1}
                             /   \
                 {getCoupon_1}   {queryCouponDiscount_1}

p_2:

                              >>= (a)
                             /   \
              {getCustomer_2}     >>= (b)
                                  /   \
                                ()    {assembleQuote_2}
```

Their merge aligns the common tree structure and preserves the hole where the
second program did no discount work:

```text
p_1 +_tree p_2:

                              >>= (a)
                             /   \
           {getCustomer_{1,2}}    >>= (b)
                                  /   \
                              >>= (c)  {assembleQuote_{1,2}}
                             /   \
                 {getCoupon_1}   {queryCouponDiscount_1}
```

The hole is not a request frontier and it is not a later phase. It is simply the
place where one branch has no work. Because that place is preserved, the later
`assembleQuote` requests remain aligned with each other instead of being pulled
into the coupon frontier.

## Requests as Frontiers

A data source batches keys of one type and returns values of one type:

```haskell
data DataSource f k a = DataSource
  { fetchMany :: NonEmpty k -> f (Map k a)
  }
```

A primitive request pairs a data source with one key:

```haskell
data Request f a where
  Request :: Ord k => DataSource f k a -> k -> Request f (Maybe a)
```

A request frontier is a free applicative over primitive requests:

```haskell
type Requests f = Ap (Request f)
```

The applicative structure records how to rebuild the result after all leaves
have returned. For example:

```haskell
(,) <$> getCustomer c1 <*> getCustomer c2
```

exposes two customer requests in one frontier. The interpreter can collect both
keys, call the customer data source once, and rebuild the pair.

## Queries as Trees

The query language separates three forms:

```haskell
data Hxl f a where
  Run  :: Requests f a -> Hxl f a
  Bind :: Hxl f b -> (b -> Hxl f a) -> Hxl f a
  Errs :: [Raised] -> Hxl f a
```

`Run` contains the next batchable request frontier. `Bind` records dependent
sequencing. `Errs` contains raised domain errors that have not yet been handled.

Pure values are pure request frontiers:

```haskell
pureHxl :: a -> Hxl f a
pureHxl = Run . pure
```

The monadic operation records a bind:

```haskell
bindHxl :: Hxl f a -> (a -> Hxl f b) -> Hxl f b
bindHxl = Bind
```

## Applicative Merge

Applicative composition is the merge for the query syntax. It is defined by the
outer constructors of the two computations. Two visible frontiers combine as
applicatives. Errors combine or propagate. A bind is followed by merging the
subprograms exposed by its continuation.

```haskell
instance Applicative (Hxl f) where
  pure = Run . pure
  (<*>) = apHxl

pair :: Hxl f a -> Hxl f b -> Hxl f (a, b)
pair = liftA2 (,)

apHxl :: Hxl f (a -> b) -> Hxl f a -> Hxl f b
apHxl ff xx =
  case (ff, xx) of
    (Errs e1, Errs e2) ->
      Errs (e1 <> e2)

    (Errs e, _) ->
      Errs e

    (_, Errs e) ->
      Errs e

    (Run rf, Run rx) ->
      Run (rf <*> rx)

    (Bind q k, Bind p h) ->
      Bind (pair q p) $ \(x, y) ->
        apHxl (k x) (h y)

    (Bind q k, p) ->
      Bind q $ \x ->
        apHxl (k x) p

    (q, Bind p h) ->
      Bind p $ \y ->
        apHxl q (h y)
```

There is no preliminary conversion to a list of phases. The merge is defined on
the syntax itself. A `Run`/`Run` pair is where batching happens. A `Bind` case
preserves the sequencing already present on one or both sides and resumes the
merge underneath it.

For conditional programs, a skipped branch is not deleted and then reconstructed
later. It remains represented by ordinary pure frontier structure, and the merge
continues at the corresponding position in the tree.

## Frontier Evaluation

Evaluation repeatedly exposes the next frontier:

```haskell
data Step f a where
  Done    :: a -> Step f a
  Blocked :: Requests f b -> (b -> Hxl f a) -> Step f a
  Failed  :: [Raised] -> Step f a
```

The stepper walks until it finds a request frontier, a value, or unhandled
errors:

```haskell
step :: Hxl f a -> Step f a
step query =
  case query of
    Errs es ->
      Failed es

    Run requests ->
      case doneRequests requests of
        Just a  -> Done a
        Nothing -> Blocked requests pureHxl

    Bind q k ->
      case step q of
        Done a ->
          step (k a)

        Failed es ->
          Failed es

        Blocked requests resume ->
          Blocked requests $ \x ->
            Bind (resume x) k
```

The runner is a loop:

```haskell
run :: Monad f => Hxl f a -> f a
run query =
  case step query of
    Done a ->
      pure a

    Failed es ->
      unhandled es

    Blocked requests resume ->
      runRequests requests >>= run . resume
```

Operationally, this says: find the currently visible request frontier, batch it,
resume the suspended programs, and repeat.

## Errors as Syntax

A checked version of the quote program may raise domain errors:

```haskell
when (expired coupon) $
  raise r (ExpiredCoupon coupon)
```

This should not be an exception in the underlying effect. It is part of the
query syntax. A raised error stores a fresh tag and a value:

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

Semantically, `channel` creates the fresh tag and handles only errors raised
through that tag:

```haskell
channel
  :: Semigroup e
  => (forall s. Raise s f e -> Hxl f a)
  -> Hxl f (Either e a)
channel body =
  freshTag $ \tag ->
    handle tag (body (Raise (\e -> Errs [Raised tag e])))
```

The handler is another tree transformation:

```haskell
handle :: Semigroup e => Tag s e -> Hxl f a -> Hxl f (Either e a)
handle tag query =
  case query of
    Run requests ->
      Run (Right <$> requests)

    Errs es ->
      case split tag es of
        ([], foreign) ->
          Errs foreign

        (local, _) ->
          pureHxl (Left (sconcat local))

    Bind q k ->
      Bind (handle tag q) $ \case
        Left e  -> pureHxl (Left e)
        Right a -> handle tag (k a)
```

The handler preserves request frontiers. Local errors become `Left`. Foreign
errors remain unhandled for an outer channel.

## Delimiter Placement

A quote program that receives a raising capability admits two placements for the delimiter:

```haskell
quote :: forall s. Raise s f QuoteError -> QuoteReq -> Hxl f Quote
```

Per-quote handling:

```haskell
traverse (\req -> channel (\raise -> quote raise req)) reqs
-- Hxl f [Either QuoteError Quote]
```

One bad quote becomes one `Left`; other quotes may still return `Right`.

Whole-batch handling:

```haskell
channel (\raise -> traverse (quote raise) reqs)
-- Hxl f (Either QuoteError [Quote])
```

All quote errors in the shared channel combine into one `Left`; success means
the whole traversal succeeded.

Only the delimiter moved. Request batching is still governed by the bind tree.
The error type appears at the boundary, not throughout the query type.

## References

- Simon Marlow et al., "There is No Fork: an Abstraction for Efficient,
  Concurrent, and Concise Data Access":
  <https://simonmar.github.io/bib/papers/haxl-icfp14.pdf>
- Haxl API documentation:
  <https://hackage.haskell.org/package/haxl-0.1.0.0/docs/Haxl-Core-Monad.html>
