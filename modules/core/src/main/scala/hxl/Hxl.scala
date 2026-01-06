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
  def andThen[B](f: A => Hxl[F, B]): Hxl[F, B] =
    Hxl.AndThen(this, f)

  def flatMapF[B](f: A => F[B])(implicit F: Functor[F]): Hxl[F, B] =
    andThen(a => Hxl.liftF(f(a)))

  def mapK[G[_]: Functor](fk: F ~> G): Hxl[G, A]

  // Provides a monadic interface for Hxl.
  // Useful for combinators such as `flatTraverse`
  def monadic: HxlM[F, A] = HxlM(this)

  def foldMap[G[_]](fk: Hxl.Compiler[F, G])(implicit G: Monad[G]): G[A] = {
    val full = Hxl.Compiler.fullCompiler(fk)
    this.tailRecM(full(_))
  }

  // runs up to the first batch and optimizes it
  //
  // note this will not run any faster than running the batch
  // but it allows doing some upfront work now
  //
  // any further composition may destroy the optimized hxl
  def optimized(implicit F: Monad[F]): Either[Hxl[F, A], F[Hxl[F, A]]]

  // Aligns this hxl, this is a hint for future composition that
  // def align: Hxl[F, A] = Hxl.align(this)

  // def alignM: HxlM[F, A] = align.monadic
}

object Hxl {
  type Target[F[_], G[_], A] = G[Either[Hxl[F, A], A]]

  type Compiler[F[_], G[_]] = NonBind[F, *] ~> Target[F, G, *]
  object Compiler {
    def fullCompiler[F[_], G[_]](compiler: Compiler[F, G])(implicit G: Applicative[G]): Hxl[F, *] ~> Target[F, G, *] =
      new (Hxl[F, *] ~> Target[F, G, *]) {
        def apply[A](fa: Hxl[F, A]): Target[F, G, A] = fa match {
          case nb: NonBind[F, A] => compiler(nb)
          case andThen: AndThen[F, a, A] =>
            val fb: G[Either[Hxl[F, a], a]] = G.unit *> apply(andThen.fa)
            fb.map {
              case Left(h)  => Left(h.andThen(andThen.fb))
              case Right(a) => Left(andThen.fb(a))
            }
        }
      }
  }

  sealed trait NonBind[F[_], A] extends Hxl[F, A]

  // Almost a free monad
  final case class Done[F[_], A](value: A) extends NonBind[F, A] {
    def mapK[G[_]: Functor](fk: F ~> G): Hxl[G, A] = Done(value)
    def optimized(implicit F: Monad[F]) = Left(this)
  }
  final case class Run[F[_], A](requests: Requests[F, A]) extends NonBind[F, A] {
    def mapK[G[_]: Functor](fk: F ~> G): Hxl[G, A] = Run(requests.mapK(fk))
    def optimized(implicit F: Monad[F]) = Left(Run(requests.optimized))
  }
  final case class LiftF[F[_], A](unFetch: F[Hxl[F, A]]) extends NonBind[F, A] {
    def mapK[G[_]: Functor](fk: F ~> G): Hxl[G, A] =
      LiftF(fk(unFetch).map(_.mapK(fk)))
    def optimized(implicit F: Monad[F]) =
      unFetch.flatMap(_.optimized.leftMap(F.pure(_)).merge).asRight
  }
  final case class AndThen[F[_], A, B](fa: Hxl[F, A], fb: A => Hxl[F, B]) extends Hxl[F, B] {
    override def mapK[G[_]: Functor](fk: F ~> G): Hxl[G, B] =
      AndThen(fa.mapK(fk), (a: A) => fb(a).mapK(fk))

    override def optimized(implicit F: Monad[F]): Either[Hxl[F, B], F[Hxl[F, B]]] =
      fa.optimized match {
        case Left(h)   => Left(AndThen(h, fb))
        case Right(fh) => Right(fh.map(h => AndThen(h, fb)))
      }
  }

  def parallelRunner[F[_]](implicit F: Parallel[F]): Compiler[F, F] = new Compiler[F, F] {
    implicit val M: Monad[F] = F.monad
    override def apply[A](fa: NonBind[F, A]): F[Either[Hxl[F, A], A]] =
      M.unit >> {
        fa match {
          case Done(a)        => M.pure(Right(a))
          case LiftF(unFetch) => unFetch.map(Left(_))
          case run: Run[F, A] => Requests.run[F, A](run.requests).map(Right(_))
        }
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
      case Some(ev) => Run[F, Option[V]](Requests.empty(source, k, Some(ev(()))))
      case None     => Run[F, Option[V]](Requests.lift(source, k))
    }

  def discard[F[_], K, V](k: K, source: DataSource[F, K, V]): Hxl[F, Unit] =
    Run[F, Unit](Requests.discard(source, k))

  def force[F[_], K: Show, V](k: K, source: DataSource[F, K, V])(implicit F: ApplicativeThrow[F]): Hxl[F, V] =
    apply[F, K, V](k, source)
      .flatMapF(F.fromOption(_, new RuntimeException(show"Key $k not found")))

  // def align[F[_], A](fa: Hxl[F, A]): Hxl[F, A] = Align(fa)

  // Almost the same signature as parallel, except we don't have a monad, but a functor instead
  // This is because of the free monad structure of Hxl, we can defer Monad evidence until we need to run
  def applicativeInstance[F[_]: Functor, G[_]: Applicative](
      fg: F ~> G,
      gf: G ~> F
  ): Applicative[Hxl[F, *]] = {
    type H[A] = Hxl[F, A]

    def ap_[A, B](n: Int, ff: H[A => B], fa: H[A]): H[B] = {
      def pseudoApply(n1: Int): Apply[H] = new Apply[H] {
        def map[A0, B0](fa: H[A0])(f: A0 => B0): H[B0] =
          fa match {
            case LiftF(fa) => LiftF(fa.map(a => pseudoApply(0).map(a)(f)))
            case Done(a)   => Done(f(a))
            case other     => ap_(n1, Done(f), other)
          }
        def ap[A0, B0](ff: H[A0 => B0])(fa: H[A0]): H[B0] = ap_(n1, ff, fa)
      }
      val reset = pseudoApply(0)
      val forward = pseudoApply(n + 1)

      val stacksafeDepth = 32

      def suspendTuple2[X, Y](x: H[X], y: H[Y]): H[(X, Y)] =
        (x, y) match {
          case (LiftF(_), _) | (_, LiftF(_)) => reset.tuple2(x, y)
          case _ =>
            if (n >= stacksafeDepth) {
              LiftF[F, (X, Y)](
                gf(Applicative[G].unit.map(_ => reset.tuple2(x, y)))
              )
            } else forward.tuple2(x, y)
        }

      (ff, fa) match {
        case (LiftF(fa), LiftF(fb)) =>
          LiftF(gf((fg(fa), fg(fb)).mapN(reset.ap(_)(_))))
        case (LiftF(fa), h) => LiftF(fa.map(reset.ap(_)(h)))
        case (h, LiftF(fa)) => LiftF(fa.map(reset.ap(h)(_)))
        case (at: AndThen[F, a1, A => B], ab: AndThen[F, a2, A]) =>
          AndThen[F, (a1, a2), B](
            suspendTuple2(at.fa, ab.fa),
            { case (a1, a2) => reset.ap(at.fb(a1))(ab.fb(a2)) }
          )

        // flatMap <*> batch -> move batch into left side of flatMap
        // to be optimistic. The choice is arbitrary.
        case (at: AndThen[F, a, A => B], fb) =>
          AndThen[F, (a, A), B](
            suspendTuple2(at.fa, fb),
            { case (a, a2) => reset.ap(at.fb(a))(Done(a2)) }
          )
        case (fa, ab: AndThen[F, a, A]) =>
          AndThen[F, (A => B, a), B](
            suspendTuple2(fa, ab.fa),
            { case (f, a2) => reset.ap(Done(f))(ab.fb(a2)) }
          )

        case (Done(f), Done(a)) => Done(f(a))
        case (r1: Run[F, A => B], r2: Run[F, A]) =>
          reset.map(Run((r1.requests, r2.requests).tupled)) { case (f, a) => f(a) }
        case (r: Run[F, A => B], Done(a)) => Run(r.requests.map(_(a)))
        case (Done(f), r: Run[F, A])      => Run(r.requests.map(f(_)))
      }
    }

    new Applicative[H] {
      def pure[A](x: A): H[A] = Done(x)
      def ap[A, B](ff: H[A => B])(fa: H[A]): H[B] = {
        ap_(0, ff, fa)
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
  implicit def monadForHxlM[F[_]]: Monad[HxlM[F, *]] = {
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
