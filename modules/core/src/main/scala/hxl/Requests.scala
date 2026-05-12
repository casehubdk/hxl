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
import cats.implicits._
import scala.collection.View
import scala.collection.mutable
import scala.collection.immutable.ArraySeq

/*
 * Requests is the accumulation structure Hxl uses internally to batch requests.
 */
final case class Requests[F[_], A](
    requests: View[(DataSource[F, ?, ?], Any)],
    rebuild: mutable.Map[DSKey[?, ?], collection.Map[Any, Any]] => A
) {
  def mapK[G[_]](fk: F ~> G) = Requests(
    requests.map { case (ds, k) => (ds.mapK(fk), k) },
    rebuild
  )

  def discard: Requests[F, Unit] = Requests(requests, _ => ())

  def optimized: Requests[F, A] = this
}
object Requests {
  def fetch[F[_], K, V](source: DataSource[F, K, V], key: K): Requests[F, Option[V]] =
    Requests(View((source, key)), m => m.get(source.key).flatMap(_.get(key).asInstanceOf[Option[V]]))

  def empty[F[_], A]: Requests[F, Unit] = Requests(View.empty, _ => throw new NoSuchElementException("empty requests"))

  implicit def applicative[F[_]] = new Applicative[Requests[F, *]] {
    def pure[A](x: A): Requests[F, A] = Requests(View.empty, _ => x)

    def ap[A, B](ff: Requests[F, A => B])(fa: Requests[F, A]): Requests[F, B] =
      Requests(ff.requests ++ fa.requests, m => ff.rebuild(m)(fa.rebuild(m)))

    override def map[A, B](fa: Requests[F, A])(f: A => B): Requests[F, B] =
      Requests(fa.requests, m => f(fa.rebuild(m)))

    override def as[A, B](fa: Requests[F, A], b: B): Requests[F, B] =
      Requests(fa.requests, _ => b)

    override def void[A](fa: Requests[F, A]): Requests[F, Unit] =
      Requests(fa.requests, _ => ())
  }

  def run[F[_]: Parallel, A](requests: Requests[F, A])(implicit F: Applicative[F]): F[A] = {
    case class Group(
        ds: DataSource[F, Any, Any],
        var keys: mutable.Set[Any]
    )
    val grouped = mutable.Map.empty[DSKey[?, ?], Group]
    requests.requests.foreach { case (ds, k) =>
      grouped.get(ds.key) match {
        case Some(grp) =>
          grp.keys += k
        case None =>
          grouped.update(ds.key, Group(ds.asInstanceOf[DataSource[F, Any, Any]], mutable.Set(k)))
      }
    }

    val fa = ArraySeq.from(grouped).parTraverse { case (_, Group(ds, keys0)) =>
      val keys = NonEmptyList.fromListUnsafe(keys0.toList)
      ds.batch(keys).tupleLeft(ds.key)
    }

    fa.map { xs =>
      val lookup = mutable.Map.empty[DSKey[?, ?], collection.Map[Any, Any]]
      xs.foreach { case (k, m) => lookup.update(k, m) }
      requests.rebuild(lookup)
    }
  }
}
