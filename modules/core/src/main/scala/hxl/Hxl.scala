/*
 * Copyright 2024 CaseHubDK
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
import cats.arrow._
import cats.implicits._

/*
 * Hxl is a value that that represents a computation that may be batched.
 * Hxl forms an applicative, and only an applicative.
 * `andThen` exists as an alternative to `flatMap`, much like `Validated`.
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

  // runs up to the first batch and optimizes it
  //
  // note this will not run any faster than running the batch
  // but it allows doing some upfront work now
  //
  // any further composition may destroy the optimized hxl
  def optimized(implicit F: Monad[F]): Either[Hxl[F, A], F[Hxl[F, A]]]

  // Aligns this hxl, this is a hint for future composition that
  def align: Hxl[F, A] = Hxl.align(this)

  def alignM: HxlM[F, A] = align.monadic
}

object Hxl {
  type Target[F[_], G[_], A] = G[Either[Hxl[F, A], A]]

  type Compiler[F[_], G[_]] = Hxl[F, *] ~> Target[F, G, *]

  // Almost a free monad
  final case class Done[F[_], A](value: A) extends Hxl[F, A] {
    def andThen[B](f: A => Hxl[F, B])(implicit F: Functor[F]): Hxl[F, B] = f(value)
    def mapK[G[_]: Functor](fk: F ~> G): Hxl[G, A] = Done(value)
    def optimized(implicit F: Monad[F]) = Left(this)
  }
  final case class Bind[F[_], A, B](
      requests: Requests[F, A],
      f: A => Hxl[F, B]
  ) extends Hxl[F, B] {
    def andThen[C](f2: B => Hxl[F, C])(implicit F: Functor[F]): Hxl[F, C] =
      Bind(requests, f.andThen(_.andThen(f2)))
    def mapK[G[_]: Functor](fk: F ~> G): Hxl[G, B] =
      Bind(requests.mapK(fk), f.andThen(_.mapK(fk)))
    def optimized(implicit F: Monad[F]) = Left(Bind(requests.optimized, f))
  }
  final case class LiftF[F[_], A](unFetch: F[Hxl[F, A]]) extends Hxl[F, A] {
    def andThen[B](f: A => Hxl[F, B])(implicit F: Functor[F]): Hxl[F, B] =
      LiftF(unFetch.map(_.andThen(f)))
    def mapK[G[_]: Functor](fk: F ~> G): Hxl[G, A] =
      LiftF(fk(unFetch).map(_.mapK(fk)))
    def optimized(implicit F: Monad[F]) =
      unFetch.flatMap(_.optimized.leftMap(F.pure(_)).merge).asRight
  }
  final case class Align[F[_], A](fa: Hxl[F, A]) extends Hxl[F, A] {
    def andThen[B](f: A => Hxl[F, B])(implicit F: Functor[F]): Hxl[F, B] =
      Align(fa.andThen(f))
    def mapK[G[_]: Functor](fk: F ~> G): Hxl[G, A] =
      Align(fa.mapK(fk))
    def optimized(implicit F: Monad[F]) =
      Left(this)

  }

  def parallelRunner[F[_]](implicit F: Parallel[F]): Compiler[F, F] = new Compiler[F, F] {
    implicit val M: Monad[F] = F.monad
    override def apply[A](fa: Hxl[F, A]): F[Either[Hxl[F, A], A]] =
      fa match {
        case Done(a)        => M.pure(Right(a))
        case Align(fa)      => M.pure(Left(fa))
        case LiftF(unFetch) => unFetch.map(Left(_))
        case bind: Bind[F, a, b] =>
          Requests
            .run[F, a](bind.requests)
            .map(bind.f)
            .map(_.asLeft)
      }
  }

  def runPar[F[_]: Parallel, A](node: Hxl[F, A]): F[A] =
    node.foldMap(parallelRunner[F])(Parallel[F].monad)

  def runSequential[F[_]: Monad, A](node: Hxl[F, A]): F[A] = {
    implicit val P: Parallel[F] = Parallel.identity[F]
    runPar(node)
  }

  def unit[F[_]]: Hxl[F, Unit] = Done(())

  def embedF[F[_], A](fa: F[Hxl[F, A]]): Hxl[F, A] = LiftF(fa)

  def liftF[F[_]: Functor, A](fa: F[A]): Hxl[F, A] = embedF(fa.map(Done(_)))

  def pure[F[_], A](a: A): Hxl[F, A] = Done(a)

  def apply[F[_], K, V](k: K, source: DataSource[F, K, V]): Hxl[F, Option[V]] =
    source.optimization match {
      case Some(ev) => Bind[F, Unit, Option[V]](Requests.discard(source, k), x => Done(Some(ev(x))))
      case None     => Bind[F, Option[V], Option[V]](Requests.lift(source, k), Done(_))
    }

  def discard[F[_], K, V](k: K, source: DataSource[F, K, V]): Hxl[F, Unit] =
    Bind[F, Unit, Unit](Requests.discard(source, k), Done(_))

  def force[F[_], K: Show, V](k: K, source: DataSource[F, K, V])(implicit F: ApplicativeThrow[F]): Hxl[F, V] =
    apply[F, K, V](k, source)
      .flatMapF(F.fromOption(_, new RuntimeException(show"Key $k not found")))

  def align[F[_], A](fa: Hxl[F, A]): Hxl[F, A] = Align(fa)

  // Almost the same signature as parallel, except we don't have a monad, but a functor instead
  // This is because of the free monad structure of Hxl, we can defer Monad evidence until we need to run
  def applicativeInstance[F[_]: Functor, G[_]: Applicative](
      fg: F ~> G,
      gf: G ~> F
  ): Applicative[Hxl[F, *]] = {
    implicit def self: Applicative[Hxl[F, *]] = applicativeInstance[F, G](fg, gf)
    type H[A] = Hxl[F, A]
    new Applicative[H] {
      def pure[A](x: A): H[A] = Done(x)
      def ap[A, B](ff: H[A => B])(fa: H[A]): H[B] =
        (ff, fa) match {
          case (LiftF(fa), LiftF(fb)) => LiftF(gf((fg(fa), fg(fb)).mapN(_ <*> _)))
          case (LiftF(fa), h)         => LiftF(fa.map(_ <*> h))
          case (h, LiftF(fa))         => LiftF(fa.map(h <*> _))

          case (Align(fa), Align(fb)) => Align(fa.ap(fb))
          // Synthetically align
          case (Align(fa), Done(fb)) => Align(fa.map(_(fb)))
          case (Done(fa), Align(fb)) => Align(fb.map(fa(_)))
          // Missing align on right side, defer fa until later
          case (Align(fa), b2: Bind[F, a2, A])      => Bind[F, a2, B](b2.requests, a2 => fa.ap(b2.f(a2)))
          case (b1: Bind[F, a1, A => B], Align(fb)) => Bind[F, a1, B](b1.requests, a1 => b1.f(a1).ap(fb))

          case (Done(f), Done(a)) => Done(f(a))
          case (b1: Bind[F, a1, A => B], b2: Bind[F, a2, A]) =>
            val comb = (b1.requests, b2.requests).tupled
            Bind[F, (a1, a2), B](comb, { case (a1, a2) => b1.f(a1) <*> b2.f(a2) })
          case (b: Bind[F, a, A => B], Done(a)) => Bind[F, a, B](b.requests, b.f(_).map(_(a)))
          case (Done(g), b: Bind[F, a, A])      => Bind[F, a, B](b.requests, b.f(_).map(g))
        }
    }
  }

  implicit def applicativeForHxl[F[_]: Applicative]: Applicative[Hxl[F, *]] =
    applicativeInstance[F, F](FunctionK.id[F], FunctionK.id[F])
}

/*
 * A monadic view of Hxl.
 * The equivalent counterpart for `Hxl` as `Either` is to `Validated`.
 */
final case class HxlM[F[_], A](hxl: Hxl[F, A]) {
  def mapK[G[_]: Functor](fk: F ~> G): HxlM[G, A] = HxlM(hxl.mapK(fk))

  def flatMapF[B](f: A => F[B])(implicit F: Functor[F]): HxlM[F, B] = HxlM(hxl.flatMapF(f))

  def foldMap[G[_]](fk: Hxl.Compiler[F, G])(implicit G: Monad[G]): G[A] = hxl.foldMap(fk)

  def applicative: Hxl[F, A] = hxl
}

object HxlM {
  def unit[F[_]]: HxlM[F, Unit] = HxlM(Hxl.unit[F])

  def liftF[F[_]: Functor, A](fa: F[A]): HxlM[F, A] = HxlM(Hxl.liftF(fa))

  def pure[F[_], A](a: A): HxlM[F, A] = HxlM(Hxl.pure(a))

  def apply[F[_], K, V](k: K, source: DataSource[F, K, V]): HxlM[F, Option[V]] =
    HxlM(Hxl(k, source))

  def force[F[_]: ApplicativeThrow, K: Show, V](k: K, source: DataSource[F, K, V]): HxlM[F, V] =
    HxlM(Hxl.force(k, source))

  def monadicK[F[_]]: Hxl[F, *] ~> HxlM[F, *] = new (Hxl[F, *] ~> HxlM[F, *]) {
    def apply[A](fa: Hxl[F, A]): HxlM[F, A] = fa.monadic
  }

  def applicativeK[F[_]]: HxlM[F, *] ~> Hxl[F, *] = new (HxlM[F, *] ~> Hxl[F, *]) {
    def apply[A](fa: HxlM[F, A]): Hxl[F, A] = fa.applicative
  }

  // Monad for HxlM
  // HxlM can implement any covariant typeclass (but not contravariant ones since `F ~> HxlM` but not `HxlM ~> F`).
  implicit def monadForHxlM[F[_]: Functor]: Monad[HxlM[F, *]] = {
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
}

object instances {

  /** A parallel instance for Hxl a bit of a footgun since (P: Parallel[F, Hxl]).monad: Monad[Hxl[F, *]], which can have very unfortunate
    * consequences if you're not careful.
    *
    * import hxl.instances.parallel._
    */
  object parallel {
    implicit def parallelForHxl[F[_]](implicit P: Parallel[F]): Parallel[Hxl[F, *]] = {
      implicit def m: Monad[F] = P.monad
      implicit def a: Applicative[P.F] = P.applicative
      type F0[A] = F[A]
      new Parallel[Hxl[F, *]] {
        type F[A] = Hxl[F0, A]
        override def sequential: F ~> F = FunctionK.id[F]
        override def parallel: F ~> F = FunctionK.id[F]
        override def applicative: Applicative[F] = Hxl.applicativeInstance[F0, P.F](P.parallel, P.sequential)
        override def monad: Monad[Hxl[F0, *]] = {
          implicit val m = P.monad
          new Monad[Hxl[F0, *]] {
            override def flatMap[A, B](fa: Hxl[F0, A])(f: A => Hxl[F0, B]): Hxl[F0, B] =
              fa.andThen(f)
            override def tailRecM[A, B](a: A)(f: A => Hxl[F0, Either[A, B]]): Hxl[F0, B] =
              a.tailRecM(f(_).monadic).hxl
            override def pure[A](x: A): Hxl[F0, A] = Hxl.Done(x)
          }
        }
      }
    }
  }

  /*
   * A parallel instance for HxlM is ambiguous.
   * Consider the difference between parallel composition of the Batch axis and the lifted effect axis
   * Which one of the following do you want:
   *   Hxl[F, A] | F[A]
   *    Batch    | Par
   *    Batch    | Seq
   *     Seq     | Par
   *     Seq     | Seq
   *
   * With Hxl (applicative) then the Hxl axis is Batch, and ap / parAp controls the effect axis
   * With HxlM (monad) then the Hxl axis is Seq and the effect axis is ambigious
   * Pick one by importing the appropriate instance:
   *  import hxl.instances.hxlm.parallel._
   *  // or
   *  import hxl.instances.hxlm.sequential._
   */
  object hxlm {
    object parallel {
      implicit def parallelHxlMForParallelEffect[G[_]](implicit P: Parallel[G]): Parallel[HxlM[G, *]] = {
        implicit def applicativePF: Applicative[P.F] = P.applicative
        implicit def monadF: Monad[G] = P.monad
        new Parallel[HxlM[G, *]] {
          type F[A] = Hxl[G, A]
          override def sequential: F ~> HxlM[G, *] = HxlM.monadicK[G]
          override def parallel: HxlM[G, *] ~> F = HxlM.applicativeK[G]
          override def applicative: Applicative[F] = Hxl.applicativeInstance[G, P.F](P.parallel, P.sequential)
          override def monad: Monad[HxlM[G, *]] = HxlM.monadForHxlM[G]
        }
      }
    }

    object sequential {
      implicit def parallelHxlMForParallelEffect[G[_]: Monad]: Parallel[HxlM[G, *]] =
        parallel.parallelHxlMForParallelEffect[G](Parallel.identity[G])
    }
  }
}
