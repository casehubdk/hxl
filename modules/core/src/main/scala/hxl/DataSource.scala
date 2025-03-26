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

import cats.data._
import cats._
import cats.implicits._

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
 *
 * Hxl eliminates duplicate keys by using scala's universal equality.
 * You can supply a custom "getKey" function if your key contains more information than what you'd like to act as "unique".
 * ```scala
 * DataSource.full(key)(x => x.id)(ks => ...)
 * ```
 */
trait DataSource[F[_], K, V] {
  type K2

  def getKey(k: K): K2

  def key: DSKey[K, V]

  def batch(ks: NonEmptyList[K]): F[Map[K2, V]]

  def mapK[G[_]](fk: F ~> G): DataSource[G, K, V] =
    DataSource.full(key)(getKey)(ks => fk(batch(ks)))
}

object DataSource {
  def full[F[_], K, K2, V](key: DSKey[K, V])(getKey: K => K2)(f: NonEmptyList[K] => F[Map[K2, V]]) = {
    type K0 = K2
    val key0 = key
    def getKey0(k: K) = getKey(k)
    new DataSource[F, K, V] {
      type K2 = K0
      def getKey(k: K) = getKey0(k)
      def key = key0
      def batch(ks: NonEmptyList[K]) = f(ks)
    }
  }

  def from[F[_], K, V](key: DSKey[K, V])(f: NonEmptyList[K] => F[Map[K, V]]) =
    full(key)(identity)(f)

  def void[F[_]: Functor, K](key: DSKey[K, Unit])(f: NonEmptyList[K] => F[Unit]): DataSource[F, K, Unit] =
    DataSource.from(key)(ks => f(ks).as(Map.empty[K, Unit]))
}
