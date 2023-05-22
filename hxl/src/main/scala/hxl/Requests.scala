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
import cats.data._
import cats.implicits._

/*
 * Requests is the accumulation structure Hxl uses internally to batch requests.
 */
sealed trait Requests[F[_], A] {
  def mapK[G[_]](fk: F ~> G): Requests[G, A]
}
object Requests {
  final case class Ap[F[_], A, B](
      left: Requests[F, A => B],
      right: Requests[F, A]
  ) extends Requests[F, B] {
    def mapK[G[_]](fk: F ~> G) = Ap(left.mapK(fk), right.mapK(fk))
  }
  final case class Lift[F[_], K, A](
      source: DataSource[F, K, A],
      key: K
  ) extends Requests[F, Option[A]] {
    def mapK[G[_]](fk: F ~> G) =
      Lift(source.mapK(fk), key)
  }
  final case class Pure[F[_], A](value: A) extends Requests[F, A] {
    def mapK[G[_]](fk: F ~> G) = Pure(value)
  }

  implicit def applicativeForRequests[F[_]]: Applicative[Requests[F, *]] = {
    type G[A] = Requests[F, A]
    new Applicative[G] {
      def pure[A](x: A): G[A] = Pure(x)
      def ap[A, B](ff: G[A => B])(fa: G[A]): G[B] = Ap(ff, fa)
    }
  }

  def getKeys[F[_]](req: Requests[F, _]): Eval[Set[Lift[F, _, _]]] = Eval.defer {
    req match {
      case Pure(_)          => Eval.now(Set.empty)
      case Ap(l, r)         => (getKeys(l), getKeys(r)).mapN(_ ++ _)
      case l: Lift[F, _, _] => Eval.now(Set(l))
    }
  }

  trait DSMap {
    def get[K, V](key: DSKey[K, V], k: K): Option[V]
  }

  def read[F[_], A](req: Requests[F, A], state: DSMap): Eval[A] = Eval.defer {
    req match {
      case Pure(a)          => Eval.now(a)
      case ap: Ap[F, a, b]  => (read(ap.left, state), read(ap.right, state)).mapN(_(_))
      case l: Lift[F, k, a] => Eval.now(state.get(l.source.key, l.key))
    }
  }

  def run[F[_]: Parallel, A](requests: Requests[F, A])(implicit
      F: Applicative[F]
  ): F[A] = {
    val keys = getKeys[F](requests).value
    final case class Key[K0, V0](key: DSKey[K0, V0])(val ds: DataSource[F, K0, V0])
    trait KeyedData[K, V] {
      def get(k: K): Option[V]
    }
    val m = keys.groupMap { case (x: Lift[F, k, v]) => Key(x.source.key)(x.source) }(_.key)
    val m2: F[List[(DSKey[_, _], KeyedData[_, _])]] = m.toList.parTraverse { case (ds: Key[k, v], keys) =>
      type K = k
      val nest: F[Map[ds.ds.K2, v]] = keys.toList.toNel match {
        case None                                 => F.pure(Map.empty[ds.ds.K2, v])
        case Some(k2: NonEmptyList[K] @unchecked) => ds.ds.batch(k2)
      }

      nest.map { m =>
        val kd = new KeyedData[k, v] {
          def get(k: k): Option[v] = m.get(ds.ds.getKey(k))
        }

        ds.key -> kd
      }
    }

    val dsMap = m2.map { xs =>
      val m3 = xs.toMap

      new DSMap {
        def get[K, V](key: DSKey[K, V], k: K): Option[V] =
          m3.get(key).flatMap { case (m: KeyedData[K, V] @unchecked) => m.get(k) }
      }
    }

    dsMap.map(read(requests, _).value)
  }
}
