# A Small Pearl for Batched Queries

## Abstract

Programs that read from services are pleasant to write as ordinary monadic
programs. A webshop quote, for example, loads a customer, optionally computes a
discount from the customer's coupon, and assembles a quote. The programmer sees
one order at a time.

The interpreter sees a different problem. It wants to batch all visible
requests: customers with customers, coupons with coupons, coupon discounts with
coupon discounts. Optional work makes this hard. A
customer with a coupon has a nested discount program; a customer without a
coupon has a hole in the same place. A flattened view of each program can shift
later requests into the wrong position.

This note presents the small idea behind Hxl: keep the computation as a bind
tree, and make applicative composition merge those trees recursively. Holes are
kept at the points where a branch did no work, so later requests keep their
relative position. Domain errors are represented in the same syntax and handled
by local delimiters.

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

The program has two paths, one for `Just` of a coupon and one for `Nothing`.
In the case of a coupon, the program's batchable functions can be written as
```text
p_1 = getCustomer -> (getCoupon -> queryCouponDiscount) -> assembleQuote
```
conversely, the `Nothing` case as

```text
p_2 = getCustomer -> () -> assembleQuote
```
which can be simplified to
```text
p_2' = getCustomer -> assembleQuote
```

## Spines and Trees
A scheduler for monad programs cannot inspect the program further than the next request.
This means that we cannot determine any information about the structure of the program beyond the next request.
Haxl solves this issue by assuming that the best option is to evaluate all ready requests immidiately.
Haxl represents programs as structures akin to cons nil lists, a spine of batch requests
```text
p_1 = getCustomer -> (getCoupon -> (queryCouponDiscount -> assembleQuote))
```
Haxl merges spines by zipping the programs, keeping the longest spine, and merging the batch requests at each step.
```text
p_1 +_spine p_2 = 
{getCustomer} -> {getCoupon, assembleQuote} -> ({queryCouponDiscount} -> {assembleQuote}))
```
Such an approach creates a plan which is not optimal. Since $p_2 \subset p_1$, then the optimal plan should be exactly $p_1$.
Even if we consider the explicit mention of the unit case, the issue remains
```text
p_1 +_spine p_2 =
{getCustomer} -> ({getCoupon, ()} -> ({queryCouponDiscount, assembleQuote} -> {assembleQuote}))
```

One novelty is to simply not remove the parentheses, as such, create a tree instead of a spine.
```text
p_1 +_tree p_2 =
                              >>= (a)
                             /   \
                {getCustomer}    >>= (b)
                                /   \
                            >>= (c)  {assembleQuote}
                           /   \         
      {getCoupon}              {queryCouponDiscount}     
+_tree
                              >>= (a)
                             /   \
                {getCustomer}    >>= (b)
                                /   \
                             () (c)  {assembleQuote}
=
                              >>= (a)
                             /   \
                {getCustomer}    >>= (b)
                                /   \
                            >>= (c)  {assembleQuote}
                           /   \         
      {getCoupon}              {queryCouponDiscount}     
```

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
have returned:

```haskell
getCustomer          :: CustomerId -> Hxl f Customer
getCoupon            :: CouponId -> Hxl f Coupon
queryCouponDiscount :: Coupon -> ProductId -> Hxl f Discount
assembleQuote       :: Customer -> ProductId -> Maybe Discount -> Hxl f Quote
```

When two independent frontiers are visible, they combine:

```haskell
(,) <$> getCustomer c1 <*> getCustomer c2
```

The interpreter can collect both customer keys, call the customer data source
once, and rebuild the pair.

## Queries as Trees

The query language separates three forms:

```haskell
data Hxl f a where
  Run  :: Requests f a -> Hxl f a
  Bind :: Hxl f b -> (b -> Hxl f a) -> Hxl f a
  Errs :: [Raised] -> Hxl f a
```

`Run` contains the next batchable request frontier. `Bind` is ordinary
dependent sequencing. `Errs` contains raised domain errors that have not yet
been handled.

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

The applicative operation is the pearl. It does not concatenate spines. It
aligns tree heads and recurses into continuations.

The following is close to the whole definition; production code still needs the
usual stack-safety and effect translation cases.

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
          Blocked requests \x ->
            Bind (resume x) k
```

The runner is then just a loop:

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

This operational story matches the checkout intuition: find all currently
visible requests, run them in batches, resume the suspended programs, and repeat.

## Errors as Syntax

A checked version of the quote program may raise domain errors:

```haskell
when (expired coupon) $
  raise r (ExpiredCoupon coupon)
```

This should not be an exception in the underlying effect. It is part of the
query. A raised error stores a fresh tag and a value:

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
errors stay unhandled for an outer channel.

## Delimiter Placement

A quote program that receives a raising capability supports two useful policies:

```haskell
quote :: forall s. Raise s f QuoteError -> QuoteReq -> Hxl f Quote
```

Per-quote handling:

```haskell
traverse (\req -> channel (\raise -> quote raise req)) reqs
-- Hxl f [Either QuoteError Quote]
```

One bad quote becomes one `Left`; other quotes can still return `Right`.

Whole-batch handling:

```haskell
channel (\raise -> traverse (quote raise) reqs)
-- Hxl f (Either QuoteError [Quote])
```

All quote errors in the shared channel combine into one `Left`; success means
the whole traversal succeeded.

Only the delimiter moved. Request batching is still governed by the bind tree.
The error type appears at the boundary, not throughout the query type.

## The Small Set of Equations

A channel catches its own raise:

```haskell
channel (\r -> raise r e)
  = pure (Left e)
```

A channel wraps success:

```haskell
channel (\_ -> pure a)
  = pure (Right a)
```

Independent raises accumulate:

```haskell
channel (\r -> (,) <$> raise r e1 <*> raise r e2)
  = pure (Left (e1 <> e2))
```

Moving the delimiter changes error scope:

```haskell
traverse (\x -> channel (\raise -> f x raise)) xs
  :: Hxl f [Either e a]

channel (\raise -> traverse (\x -> f x raise) xs)
  :: Hxl f (Either e [a])
```

The batching idea is orthogonal: frontier discovery follows `Run` and `Bind`;
error handling follows `Errs` and delimiters.

## What Hxl Buys

The checkout program began as ordinary monadic code:

```text
getCustomer -> (getCoupon -> queryCouponDiscount) -> assembleQuote
```

The no-coupon case has a hole:

```text
getCustomer -> () -> assembleQuote
```

A flattened spine cannot tell whether a later step moved earlier or merely
skipped optional work. Hxl keeps the bind tree:

```text
coupon:    ((getCustomer, (getCoupon, queryCouponDiscount)), assembleQuote)
no coupon: ((getCustomer, ()),                               assembleQuote)
```

That one structural choice gives the scheduler enough information to expose
batch frontiers without losing alignment. Delimited errors use the same syntax:
raise nodes are ordinary query nodes, and channels are ordinary tree
transformations.

The result is small: one request frontier, one bind tree, one frontier
evaluator, and one delimited error handler.

## References

- Simon Marlow et al., "There is No Fork: an Abstraction for Efficient,
  Concurrent, and Concise Data Access":
  <https://simonmar.github.io/bib/papers/haxl-icfp14.pdf>
- Haxl API documentation:
  <https://hackage.haskell.org/package/haxl-0.1.0.0/docs/Haxl-Core-Monad.html>
