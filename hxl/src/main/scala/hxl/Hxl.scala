/*
 * Copyright 2023 CaseHubDK
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package hxl

import cats._
import cats.implicits._

/*
 * Hxl is a value that that represents a computation that may be batched.
 * Hxl forms an applicative, and only an applicative.
 * `andThen` exists as an alternative to `flatMap` (since that wouldn't be lawful), much like `Validated`.
 */
sealed trait Hxl[F[_], A] {
  def andThen[B](f: A => Hxl[F, B])(implicit F: Functor[F]): Hxl[F, B]

  def flatMapF[B](f: A => F[B])(implicit F: Functor[F]): Hxl[F, B] =
    andThen(a => Hxl.liftF(f(a)))

  def mapK[G[_]: Functor](fk: F ~> G): Hxl[G, A]

  // Provides a monadic interface for Hxl.
  // Useful for combinators such as `flatTraverse`
  def monadic: HxlM[F, A] = HxlM(this)

  def foldMap[G[_]](fk: Hxl.Compiler[F, G])(implicit G: Monad[G]): G[A] =
    this.tailRecM(fk(_))
}

object Hxl {
  type Target[F[_], G[_], A] = G[Either[Hxl[F, A], A]]

  type Compiler[F[_], G[_]] = Hxl[F, *] ~> Target[F, G, *]

  // Almost a free monad
  final case class Done[F[_], A](value: A) extends Hxl[F, A] {
    def andThen[B](f: A => Hxl[F, B])(implicit F: Functor[F]): Hxl[F, B] = f(value)
    def mapK[G[_]: Functor](fk: F ~> G): Hxl[G, A] = Done(value)
  }
  final case class Bind[F[_], A, B](
      requests: Requests[F, A],
      f: A => Hxl[F, B]
  ) extends Hxl[F, B] {
    def andThen[C](f2: B => Hxl[F, C])(implicit F: Functor[F]): Hxl[F, C] =
      Bind(requests, f.andThen(_.andThen(f2)))
    def mapK[G[_]: Functor](fk: F ~> G): Hxl[G, B] =
      Bind(requests.mapK(fk), f.andThen(_.mapK(fk)))
  }
  final case class LiftF[F[_], A](unFetch: F[Hxl[F, A]]) extends Hxl[F, A] {
    def andThen[B](f: A => Hxl[F, B])(implicit F: Functor[F]): Hxl[F, B] =
      LiftF(unFetch.map(_.andThen(f)))
    def mapK[G[_]: Functor](fk: F ~> G): Hxl[G, A] =
      LiftF(fk(unFetch).map(_.mapK(fk)))
  }

  def parallelRunner[F[_]](implicit F: Parallel[F]): Compiler[F, F] = new Compiler[F, F] {
    implicit val M: Monad[F] = F.monad
    override def apply[A](fa: Hxl[F, A]): F[Either[Hxl[F, A], A]] =
      fa match {
        case Done(a)        => M.pure(Right(a))
        case LiftF(unFetch) => unFetch.map(Left(_))
        case bind: Bind[F, a, b] =>
          Requests
            .run[F, a](bind.requests)
            .map(bind.f)
            .map(_.asLeft)
      }
  }

  def runPar[F[_]: Parallel, A](node: Hxl[F, A])(implicit F: Monad[F]): F[A] =
    node.foldMap(parallelRunner[F])

  def runSequential[F[_]: Monad, A](node: Hxl[F, A]): F[A] = {
    implicit val P: Parallel[F] = Parallel.identity[F]
    runPar(node)
  }

  def embedF[F[_], A](fa: F[Hxl[F, A]]): Hxl[F, A] = LiftF(fa)

  def liftF[F[_]: Functor, A](fa: F[A]): Hxl[F, A] = embedF(fa.map(Done(_)))

  def pure[F[_], A](a: A): Hxl[F, A] = Done(a)

  def apply[F[_], K, V](k: K, source: DataSource[F, K, V]): Hxl[F, Option[V]] =
    Bind[F, Option[V], Option[V]](Requests.Lift(source, k), Done(_))

  def force[F[_], K: Show, V](k: K, source: DataSource[F, K, V])(implicit F: ApplicativeThrow[F]): Hxl[F, V] =
    apply[F, K, V](k, source)
      .flatMapF(F.fromOption(_, new RuntimeException(show"Key $k not found")))

  implicit def parallelForHxl[F[_]](implicit P: Parallel[F]): Parallel[Hxl[F, *]] = {
    type G[A] = Hxl[F, A]
    new Parallel[G] {
      type F[A] = Hxl[P.F, A]

      override def sequential: F ~> G = new (F ~> G) {
        def apply[A](fa: F[A]): G[A] = fa.mapK(P.sequential)(P.monad)
      }
      override def parallel: G ~> F = new (G ~> F) {
        def apply[A](fa: G[A]): F[A] = fa.mapK(P.parallel)(P.applicative)
      }

      override def applicative: Applicative[F] = applicativeForHxl[P.F](P.applicative)

      override def monad: Monad[G] = {
        implicit val m = P.monad
        new Monad[G] {
          override def flatMap[A, B](fa: G[A])(f: A => G[B]): G[B] = fa.monadic.flatMap(f(_).monadic).hxl
          override def tailRecM[A, B](a: A)(f: A => G[Either[A, B]]): G[B] = a.tailRecM(f(_).monadic).hxl
          override def pure[A](x: A): G[A] = Done(x)
        }
      }
    }
  }

  implicit def applicativeForHxl[F[_]: Applicative]: Applicative[Hxl[F, *]] = {
    type G[A] = Hxl[F, A]
    new Applicative[G] {
      def pure[A](x: A): G[A] = Done(x)
      def ap[A, B](ff: G[A => B])(fa: G[A]): G[B] =
        (ff, fa) match {
          case (LiftF(fa), LiftF(fb)) => LiftF((fa, fb).mapN(_ <*> _))
          case (LiftF(fa), h)         => LiftF(fa.map(_ <*> h))
          case (h, LiftF(fa))         => LiftF(fa.map(h <*> _))
          case (Done(f), Done(a))     => Done(f(a))
          case (b1: Bind[F, a1, A => B], b2: Bind[F, a2, A]) =>
            val comb = (b1.requests, b2.requests).tupled
            Bind[F, (a1, a2), B](comb, { case (a1, a2) => b1.f(a1) <*> b2.f(a2) })
          case (b: Bind[F, a, A => B], Done(a)) => Bind[F, a, B](b.requests, b.f(_).map(_(a)))
          case (Done(g), b: Bind[F, a, A])      => Bind[F, a, B](b.requests, b.f(_).map(g))
        }
    }
  }
}

/*
 * A monadic view of Hxl.
 * The equivalent counterpart for `Hxl` as `Either` is to `Validated`.
 * Is effectively the identity monad transformer.
 */
final case class HxlM[F[_], A](hxl: Hxl[F, A]) {
  def mapK[G[_]: Functor](fk: F ~> G): HxlM[G, A] = HxlM(hxl.mapK(fk))

  def flatMapF[B](f: A => F[B])(implicit F: Functor[F]): HxlM[F, B] = HxlM(hxl.flatMapF(f))
}

object HxlM {
  // Monad for HxlM
  // HxlM can implement any covariant typeclass (but not contravariant ones since `F ~> HxlM` but not `HxlM ~> F`).
  implicit def monadForHxlM[F[_]: Monad]: Monad[HxlM[F, *]] = {
    type G[A] = HxlM[F, A]
    new Monad[G] {
      override def pure[A](x: A): G[A] = HxlM(Hxl.Done(x))
      override def flatMap[A, B](fa: G[A])(f: A => G[B]): G[B] = HxlM(fa.hxl.andThen(f(_).hxl))
      override def tailRecM[A, B](a: A)(f: A => G[Either[A, B]]): G[B] =
        HxlM {
          f(a).hxl.andThen {
            case Left(a)  => tailRecM(a)(f).hxl
            case Right(b) => pure(b).hxl
          }
        }
    }
  }

  /*
   * A parallel instance for HxlM is dangerously ambiguous.
   * Consider the difference between parallel composition of the Batch axis and the lifted effect axis
   * Which one of the following do you want:
   *   Hxl[F, A] | F[A]
   *    Batch    | Seq
   *    Batch    | Par
   *     Seq     | Seq
   *     Seq     | Par
   *
   * With Hxl (applicative) then the Hxl axis is Batch, and ap / parAp controls the effect axis
   * With HxlM (monad) then the Hxl axis is Seq and the effect axis is ambigious
   * If you need a parallel instance for Hxl consider implementing one ad-hoc, here is an example:
   * ```scala
   * implicit def parallelForHxlM[G[_]: Monad]: Parallel[HxlM[G, *]] = new Parallel[HxlM[G, *]] {
   *   type F[A] = Hxl[G, A]
   *   override def sequential: F ~> HxlM[G, *] = FunctionK.liftFunction(HxlM(_))
   *   override def parallel: HxlM[G, *] ~> F = FunctionK.liftFunction(_.hxl)
   *   override def applicative: Applicative[F] = Hxl.applicativeForHxl[G]
   *   override def monad: Monad[HxlM[G, *]] = monadForHxlM[G]
   * }
   * ```
   */
}
