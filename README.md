# Hxl <a href="https://typelevel.org/cats/"><img src="https://typelevel.org/cats/img/cats-badge.svg" height="40px" align="right" alt="Cats friendly" /></a>
Hxl is a pure applicative batching library for Scala.

Hxl is based on the ideas presented in [Haxl](https://simonmar.github.io/bib/papers/haxl-icfp14.pdf), but diverges in in a few ways.
Notably, Hxl does not perform side effects, but is instead based on a free applicative structure.

Hxl is very small (only a couple hundred lines of code) and only depends on cats.

Hxl is written in tagless final, which allows for molding the library to your needs.

## Installation
Hxl is available on Maven Central for Scala 2.13 and 3.2.
```scala
libraryDependencies += "com.github.casehubdk" %% "hxl" % "0.2.3"
```

## Usage
There are two primitive structures in Hxl: `DataSource[F, K, V]` and `DSKey[K, V]`.
A `DataSource[F, K, V]` abstracts over a function `NonEmptyList[K] => F[Map[K, V]]`.
A `DataSource` is uniquely (as in Scala universal equals) identified by its `DSKey`.
```scala
import cats._
import cats.implicits._

final case class MyDSKey(id: String)
case object MyDSKey extends DSKey[MyDSKey, String]

val database = Map(
  "foo" -> "bar",
  "baz" -> "qux"
)

def dataSource[F[_]: Applicative] = DataSource.from[F, MyDSKey, String](MyDSKey) { keys =>
  keys.toList.flatMap(key => database.get(key.id).toList.tupleLeft(key)).toMap.pure[F]
}

val fa: Hxl[Id, Option[String]] = Hxl(MyDSKey("foo"), dataSource[Id])
val fb: Hxl[Id, Option[String]] = Hxl(MyDSKey("baz"), dataSource[Id])
Hxl.runSequential((fa, fb).mapN(_.mkString + " " + _.mkString)) // "bar qux"
```

Hxl forms an applicative, but sometimes you need a monad.
Hxl is like `Validated` from `cats`, in that it can escape it's applicative nature via a method `andThen`.
However, if you need Hxl to become a monad (like `Either` is to `Validated`), you can use request a monadic view of your effect:
```scala
val fa: Hxl[F, String] = ???

val m: HxlM[F, String] = fa.monadic.flatMap{ x =>
  ???
}

val back: Hxl[F, String] = m.hxl
```

## Advanced usage
Since Hxl is written in tagless final, you can add various behaviors to your data sources.
For instance, you can add (pure) caching.
```scala
import cats.data._
import cats.implicits._

final case class MyDSKey(id: String)
case object MyDSKey extends DSKey[MyDSKey, String]

val database = Map(
  "foo" -> "bar",
  "baz" -> "qux"
)

type Cache = Map[String, String]
type Effect[A] = State[Cache, A]

def dataSource: DataSource[Effect, MyDSKey, String] = DataSource.from(MyDSKey) { keys =>
  State[Cache, Map[MyDSKey, String]] { cache =>
    val (misses, hits) = keys.toList.partitionEither(k => cache.get(k.id).tupleLeft(k).toRight(k))
    val fetched = misses.flatMap(key => database.get(key.id).toList.map(key -> _)).toMap
    (cache ++ fetched.map{ case (k, v) => k.id -> v }, hits.toMap ++ fetched)
  }
}
```

## Extending Hxl
Hxl's interface is public and small, so extension is very possible.

Under the hood, Hxl compiles your structure into `F[A]` via a natural transformation:
```scala
type Target[F[_], G[_], A] = G[Either[Hxl[F, A], A]]

type Compiler[F[_], G[_]] = Hxl[F, *] ~> Target[F, G, *]
```
Hxl repeats your natural transformation until the result becomes `Right`, like `tailRecM`.
```scala
def fa: Hxl[F, A] = ???

fa.foldMap(Hxl.parallelRunner[F]): F[A]
```
`Compiler`s can be composed like ordinary functions such that the core of Hxl is exposed for extension.

As an example, let's add tracing (from `natchez`) to Hxl:
```scala
import natchez._
import cats._
import cats.data._
import cats.implicits._

def traceRequests[F[_]: Trace: Applicative, A](req: Requests[F, A]): Requests[F, A] = req match {
  case Requests.Pure(value)     => Requests.Pure(value)
  case ap: Requests.Ap[F, a, b] => Requests.Ap(traceRequests(ap.left), traceRequests(ap.right))
  case lift: Requests.Lift[F, k, a] =>
    val newSource = DataSource.full[F, k, lift.source.K2, a](lift.source.key)(lift.source.getKey) { ks =>
      Trace[F].span(s"datasource.${lift.source.key}") {
        Trace[F].put("keys" -> ks.size.toString) *> lift.source.batch(ks)
      }
    }

    Requests.Lift(newSource, lift.key)
}

def composeTracing[F[_]: Trace: Applicative, G[_]: Trace: Applicative](
    compiler: Compiler[F, G]
): Compiler[F, StateT[G, Int, *]] = {
  type Effect[A] = StateT[G, Int, A]
  new Compiler[F, Effect] {
    def apply[A](fa: Hxl[F, A]): Hxl.Target[F, Effect, A] =
      fa match {
        case Hxl.LiftF(unFetch) =>
          StateT.liftF {
            Trace[G].span("hxl.fetch") {
              compiler(Hxl.LiftF(unFetch))
            }
          }
        case bind: Hxl.Bind[F, a, b] =>
          StateT { round: Int =>
            Trace[G]
              .span("hxl.bind") {
                Trace[G].put("round" -> round.toString) *> compiler {
                  Hxl.Bind(traceRequests(bind.requests), bind.f)
                }
              }
              .map(round + 1 -> _)
          }
        case other => StateT.liftF(compiler(other))
      }
  }
}

def fa: Hxl[F, String] = ???

val result: F[String] = fa.foldMap(composeTracing[F, F](Hxl.parallelRunner)).runA(0)
```

