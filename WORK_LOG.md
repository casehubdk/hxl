# Hxl Work Log

Snapshot date: 2026-05-22.

## Project Direction

Recent commits show three active threads:

- Presentation work: `PRESENTATION.md`, `PRESENTATION_V2.md`, `PRESENTATION_V3.md`.
- Core simplification and safety: changes around `Hxl`, `Requests`, channels, tests.
- Fast traversal/evaluation work: `HxlOpt`, `DataSource`, `Requests`, and evaluation tests.

The current branch is `error-channel`. The last 10 commit subjects were:

```text
01b8387 embedding
0d6bbeb safety
43d1aa8 fast
241ecf0 v3
9ee127e tupilak
55adaf3 work
f1f941a example
a836d32 more simple
c3c8fb0 work
53e628e presentation
```

Best read: the project is moving toward a small, presentable Hxl core with clear safety/error behavior, a faster traversal path, and persuasive examples that explain why Hxl is useful beyond raw batching.

## JMH Benchmark Work

Added a `bench` module:

- `modules/bench/src/main/scala/hxl/bench/HxlTraverseBenchmark.scala`
- `project/plugins.sbt` adds `sbt-jmh`
- `build.sbt` adds `bench`
- benchmark uses `cats-effect` `3.7.0`
- benchmark uses `SyncIO`
- benchmark runs in JMH `Mode.Throughput`
- output unit is ops/sec, so higher score is better

Benchmarked variants:

- `catsTraverseHxl`: `xs.traverse(hxlProgram)` then `Hxl.runSequential`
- `fastTraverseHxl`: `Hxl.traverse(xs)(hxlProgram)` then `Hxl.runSequential`
- `manualMapBatch`: manual stage batching returning `Map`
- `manualPairedBatch`: manual stage batching returning paired input/output list

Current benchmark shape:

- `size`: `100`, `1000`, `10000`
- `depth`: `1`, `2`, `3`, `5`, `10`
- `Payload(id, path)`
- every stage prepends with `stage :: payload.path`
- datasource uses `DataSource.full`
- datasource returns `Map.from(keys.map(payload => payload -> step(stage, payload)))`
- manual map path uses `Map.from(inputs.map(payload => payload -> step(stage, payload)))`
- no hashmap-specific implementation shortcut
- no `.iterator` plumbing in datasource

Run command for compile:

```bash
sbt bench/Jmh/compile
```

Short mid-range run, roughly 20-30 seconds depending on machine:

```bash
sbt 'bench/Jmh/run -wi 3 -i 3 -f1 -t1 -w 1s -r 1s -p size=1000 -p depth=3 hxl.bench.HxlTraverseBenchmark'
```

Previously verified:

- `sbt core/test bench/Jmh/compile`
- short JMH smoke run

## Benchmark Interpretation

Throughput means ops/sec. Higher score is better.

Manual batching can beat Hxl in this synthetic benchmark because it is a lower-bound implementation:

- it does one tight pass per stage
- it has no Hxl interpreter overhead
- it has no request graph construction overhead
- it has no general datasource machinery beyond the benchmark code

That does not invalidate Hxl. It means this benchmark mostly measures raw in-memory batching overhead.

The more realistic Hxl advantage is structural:

```scala
x <- runSomeStep(y)
z <- runNextStep(x)
```

The result `x` is bound once and reused in lexical scope. In manual map batching, later stages usually rebuild keys and reconnect context with maps or row structures. Missing keys and error handling make that plumbing worse.

## Unsafe Fetch Work

Added:

```scala
Requests.unsafeFetch(source, key)
Hxl.unsafeGet(key, source)
```

Shape:

```scala
def unsafeFetch[F[_], K, V](source: DataSource[F, K, V], key: K): Requests[F, V] =
  Requests { setup =>
    val value = setup.request(source, key)
    () => value().get
  }
```

`Hxl.unsafeGet` delegates to `Requests.unsafeFetch`.

Tests added:

- `Hxl.unsafeGet` returns existing values
- `Hxl.unsafeGet` throws on missing values
- `Requests.unsafeFetch` returns existing values

## Quote Blueprint

Added `QUOTE_BLUEPRINT.scala`.

This is intentionally blueprint code, not intended to compile. It sketches a realistic `getQuote` problem where Hxl keeps business dependencies readable and manual batching exposes the plumbing.

The scenario:

- validate quote request
- load customer
- load account
- load cart
- validate cart
- load shipping address
- price each cart line
- apply coupon discount when allowed
- quote shipping
- quote tax
- assemble final quote

Pure code stays pure:

- `validateQuoteReq`
- `validateCart`
- `expectValidCart`
- `canApplyCoupon`
- `assembleQuote`
- small case-class methods like `ValidCart.lines` and `PricedLine.subtotal`

Hxl code keeps the flow monadic:

```scala
customer <- getCustomer(req.customerId).monadic
account <- getAccount(customer.accountId).monadic
cart <- getCart(req.cartId).map(expectValidCart).monadic
address <- getAddress(req.shipTo).monadic
lines <- cart.lines.traverse(line => priceLine(line, customer, account)).monadic
discounts <- couponDiscounts(req, customer, account, lines).monadic
shipping <- quoteShipping(ShippingInput(address, account, lines)).monadic
tax <- quoteTax(TaxInput(address, customer, lines, shipping)).monadic
```

Current helper subroutines:

- `priceLine`
- `couponDiscounts`
- `assembleQuote`

Removed on purpose:

- `QuoteContext`
- tupled initial load
- `shippingAndTax`
- one-line map helpers like `customerFor`, `accountFor`, `priceInput`, `shippingInput`, `taxInput`

## Manual Map Formulation

`getQuotesManually` models the same business shape using map-returning batch APIs.

It deliberately stays semantically equivalent to the Hxl sketch:

- customers by `CustomerId`
- accounts by `AccountId`
- carts by `CartId`
- valid carts
- addresses by `AddressId`
- product and inventory by `Sku`
- prices by `QuoteLineInput`
- coupon discounts by `CouponInput`
- shipping by `ShippingInput`
- tax by `TaxInput`

This version shows the map pain directly:

- repeated reconstruction of inputs
- map lookups at every derived stage
- quote context recovered repeatedly
- shipping and tax inputs rebuilt because they are map keys
- missing-key handling omitted, but would add more noise in real code

## Ordered List Formulation

`getQuotesWithLists` models batch APIs that return ordered `List[V]` matching input order.

It avoids map lookups, but must carry row context through every stage:

- quote rows
- line rows
- line rows plus catalog data
- priced lines
- quote rows plus priced lines
- optional coupon slots
- quote rows plus discounts
- quote rows plus shipping
- quote rows plus tax

The list version is fairer than intentionally bad code, but it shows a different kind of pain:

- deep tuple rows
- repeated zipping
- chunking flat line results back per quote with `splitAt`
- optional coupon outputs consumed in order
- positional correctness becomes part of the business code

## Current Worktree Notes

Staged changes currently include:

- `QUOTE_BLUEPRINT.scala`
- `build.sbt`
- `project/plugins.sbt`
- `modules/bench/src/main/scala/hxl/bench/HxlTraverseBenchmark.scala`
- `modules/core/src/main/scala/hxl/Hxl.scala`
- `modules/core/src/main/scala/hxl/Requests.scala`
- `modules/core/src/test/scala/hxl/HxlEvaluationTest.scala`

No commit has been made for this work in the current session.

This file, `WORK_LOG.md`, is newly created and untracked until added.

## Open Caveats

- `QUOTE_BLUEPRINT.scala` is a sketch file, not production code.
- Manual map and list versions omit realistic missing-key/error handling.
- `unsafeFetch` uses `Option.get`; useful for benchmark and unsafe ergonomics, but not a typed error API.
- Benchmark is useful for overhead comparison, not proof of real application performance.
- A stronger real-world benchmark would model multiple dependent lookups where intermediate results are reused several times.
