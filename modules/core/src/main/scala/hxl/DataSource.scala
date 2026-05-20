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
import cats.implicits._
import scala.collection.mutable
import scala.reflect.ClassTag

/*
 * A data source is simply a pair of a key and a function that efficiently fetches data.
 * Note that it is legal for a data source to be instantiated many times with the same key.
 * In fact, this is the recommended way to use Hxl, as it allows for more flexibility:
 * ```scala
 * // Stable key reference (equality)
 * val key = DSKey[String, String]
 * // Cannot have a stable reference, since it has dependencies.
 * def dataSource[F[_]: Concurrent]: DataSource[F, String, String] = DataSource.from(...
 * ```
 *
 * Maybe you want to seperate data sources by organization
 * ```scala
 * final case class ValueKey(id: String)
 * final case class Key(org: String) extends DSKey[ValueKey, String]
 * def dataSource[F[_]: Concurrent](org: String): DataSource[F, ValueKey, String] =
 *   DataSource.from(Key(org)){ ks => ...
 * ```
 */
trait DataSource[F[_], K, V] {
  def key: DSKey[K, V]

  def batch(ks: mutable.ArrayBuffer[K]): F[DataSource.Result[K, V]]

  def mapK[G[_]](fk: F ~> G): DataSource[G, K, V] =
    DataSource.fullResult[G, K, V](key)(ks => fk(batch(ks)))
}

object DataSource {
  trait Result[K, +V] {
    def get(key: K, index: Int): Option[V]
  }

  object Result {
    def empty[K, V]: Result[K, V] =
      EmptyResult.asInstanceOf[Result[K, V]]

    def fromMap[K, V](values: collection.Map[K, V]): Result[K, V] =
      new MapResult(values)

    def fromIterableOnce[K, V](values: IterableOnce[(K, V)]): Result[K, V] =
      fromMap(scala.collection.mutable.HashMap.from(values))

    def indexedValues[K, V](values: Array[V]): Result[K, V] =
      new IndexedValueResult(values)
  }

  private object EmptyResult extends Result[Any, Nothing] {
    def get(key: Any, index: Int): Option[Nothing] = None
  }

  private final class MapResult[K, V](values: collection.Map[K, V]) extends Result[K, V] {
    def get(key: K, index: Int): Option[V] =
      values.get(key)
  }

  private final class IndexedValueResult[K, V](values: Array[V]) extends Result[K, V] {
    def get(key: K, index: Int): Option[V] =
      if (index >= 0 && index < values.length) Some(values(index))
      else None
  }

  def fullResult[F[_], K, V](key: DSKey[K, V])(f: mutable.ArrayBuffer[K] => F[Result[K, V]]) = {
    val key0 = key
    new DataSource[F, K, V] {
      def key = key0
      def batch(ks: mutable.ArrayBuffer[K]) = f(ks)
    }
  }

  def full[F[_]: Functor, K, V](key: DSKey[K, V])(f: mutable.ArrayBuffer[K] => F[collection.Map[K, V]]) =
    fullResult(key)(ks => f(ks).map(Result.fromMap(_)))

  final class PartiallyAppliedIt[F[_], K, V](key: DSKey[K, V]) {
    def apply[Container <: IterableOnce[(K, V)]](
        f: mutable.ArrayBuffer[K] => F[Container]
    )(implicit F: Functor[F]): DataSource[F, K, V] =
      fullResult(key)(ks => f(ks).map(Result.fromIterableOnce(_)))
  }

  def from[F[_], K, V](key: DSKey[K, V]): PartiallyAppliedIt[F, K, V] =
    new PartiallyAppliedIt(key)

  def assoc[F[_]: Functor, K: ClassTag, V](key: DSKey[K, V])(f: Array[K] => F[Array[V]]): DataSource[F, K, V] =
    fullResult(key) { ks =>
      val keys = ks.toArray
      f(keys).map(Result.indexedValues[K, V])
    }

  def void[F[_]: Functor, K](key: DSKey[K, Unit])(f: mutable.ArrayBuffer[K] => F[Unit]): DataSource[F, K, Unit] =
    DataSource.fullResult(key)(ks => f(ks).as(Result.empty[K, Unit]))
}
