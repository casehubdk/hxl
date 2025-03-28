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
import cats.data._
import cats.free._
import cats.implicits._
import cats.arrow.FunctionK
import scala.collection.View

/*
 * Requests is the accumulation structure Hxl uses internally to batch requests.
 */
final case class Requests[F[_], A](
    discards: View[Requests.Discarded[F, ?]],
    assocs: FreeApplicative[Requests.Assoc[F, *], A],
    private val cachedRequests: Option[Requests.PreparedRequests[F]]
) {
  import Requests._
  def mapK[G[_]](fk: F ~> G) = Requests(
    discards.map(_.mapK(fk)),
    assocs.compile(new FunctionK[Assoc[F, *], Assoc[G, *]] {
      def apply[B](fa: Assoc[F, B]): Assoc[G, B] = fa.mapK(fk)
    }),
    cachedRequests.map(_.map { case (k, t) => (k, t.copy(source = t.source.mapK(fk))) })
  )

  def visit(visitor: DataSourceVisitor[F]): Requests[F, A] =
    Requests(
      discards.map { case d: Discarded[F, a] =>
        val (source, key) = visitor.visit(d.source, d.key)
        Discarded[F, a](source, key)
      },
      assocs.compile(new FunctionK[Assoc[F, *], Assoc[F, *]] {
        def apply[B](fa: Assoc[F, B]): Assoc[F, B] = fa match {
          case a: AssocImpl[F, k, b] @unchecked =>
            val (source, key) = visitor.visit(a.source, a.key)
            AssocImpl[F, k, b](source, key)
        }
      }),
      cachedRequests.map(_.map { case (k, t) =>
        val (source, _) = visitor.visit(t.source, t.keys.head)
        (k, T(source, t.keys))
      })
    )

  def optimized: Requests[F, A] =
    cachedRequests match {
      case Some(_) => this
      case None    => copy(cachedRequests = Some(generateRequestKeys(this)))
    }
}
object Requests {
  trait DataSourceVisitor[F[_]] {
    def visit[K, V](source: DataSource[F, K, V], k: K): (DataSource[F, K, V], K)
  }

  final case class Discarded[F[_], K](source: DataSource[F, K, ?], key: K) {
    def mapK[G[_]](fk: F ~> G) = Discarded(source.mapK(fk), key)
  }

  sealed trait Assoc[F[_], A] {
    def mapK[G[_]](fk: F ~> G): Assoc[G, A]
  }
  final case class AssocImpl[F[_], K, A](source: DataSource[F, K, A], key: K) extends Assoc[F, Option[A]] {
    def mapK[G[_]](fk: F ~> G) = AssocImpl(source.mapK(fk), key)
  }

  implicit def applicativeForBase[F[_]]: Applicative[Requests[F, *]] = new Applicative[Requests[F, *]] {
    def pure[A](x: A): Requests[F, A] = Requests(View.empty, FreeApplicative.pure(x), None)

    val FF = FreeApplicative.freeApplicative[Assoc[F, *]]
    def ap[A, B](ff: Requests[F, A => B])(fa: Requests[F, A]): Requests[F, B] =
      Requests(ff.discards ++ fa.discards, FF.ap(ff.assocs)(fa.assocs), None)
  }

  def lift[F[_], A, B](source: DataSource[F, A, B], key: A): Requests[F, Option[B]] = {
    val a: Assoc[F, Option[B]] = AssocImpl[F, A, B](source, key)
    Requests(View.empty, FreeApplicative.lift(a), None)
  }

  def discard[F[_], A](source: DataSource[F, A, ?], key: A): Requests[F, Unit] =
    Requests(View(Discarded(source, key)), FreeApplicative.pure(()), None)

  final case class DSKey0(value: Any) extends AnyRef
  final case class ValueKey(value: Any) extends AnyRef
  final case class Value(value: Any) extends AnyRef
  final case class T2(result: Map[Any, Any]) extends AnyVal
  final case class T[F[_]](source: DataSource[F, Any, Any], keys: scala.collection.mutable.Set[ValueKey])
  type PreparedRequests[F[_]] = Seq[(DSKey0, T[F])]
  def generateRequestKeys[F[_], A](reqs: Requests[F, A]): PreparedRequests[F] = {
    val xs = scala.collection.mutable.HashMap.empty[DSKey0, T[F]]
    val c = Const(())
    reqs.assocs.foldMap(new FunctionK[Assoc[F, *], Const[Unit, *]] {
      def apply[B](fa: Assoc[F, B]): Const[Unit, B] = {
        val ai = fa.asInstanceOf[AssocImpl[F, A, ?]]
        val t = xs.getOrElseUpdate(
          DSKey0(ai.source.key),
          T(ai.source.asInstanceOf[DataSource[F, Any, Any]], scala.collection.mutable.Set.empty[ValueKey])
        )
        t.keys += ValueKey(ai.key)
        c.retag[B]
      }
    })
    reqs.discards.foreach { case (d: Discarded[F, a]) =>
      val t = xs.getOrElseUpdate(
        DSKey0(d.source.key),
        T(d.source.asInstanceOf[DataSource[F, Any, Any]], scala.collection.mutable.Set.empty[ValueKey])
      )
      t.keys += ValueKey(d.key)
    }
    xs.toSeq
  }

  def run[F[_]: Parallel, A](requests: Requests[F, A])(implicit
      F: Applicative[F]
  ): F[A] = {
    val xs = generateRequestKeys(requests)

    val ops = xs.toSeq.parTraverse { case (_, t) =>
      t.keys.view
        .map(_.value)
        .toList
        .toNel
        .traverse(nel => t.source.batch(nel))
        .map(View.from(_).map(m => (DSKey0(t.source.key), T2(m.asInstanceOf[Map[Any, Any]]))))
    }

    ops.map { v =>
      val m = v.view.flatten.toMap
      requests.assocs.foldMap[Id](new FunctionK[Assoc[F, *], Id] {
        def apply[B](fa: Assoc[F, B]): Id[B] = fa match {
          case ai: AssocImpl[F, k, a] @unchecked =>
            m.get(DSKey0(ai.source.key)).flatMap(_.result.get(ai.source.k2(ai.key))).asInstanceOf[B]
        }
      })
    }
  }
}
