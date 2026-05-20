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
import scala.collection.immutable.ArraySeq

/*
 * Requests is the accumulation structure Hxl uses internally to batch requests.
 */
final case class Requests[F[_], A](
    setup: Requests.Setup[F] => () => A
) {
  def mapK[G[_]](fk: F ~> G): Requests[G, A] =
    Requests(setup0 => setup(new Requests.MapKSetup(setup0, fk)))

  def discard: Requests[F, Unit] =
    Requests { setup0 =>
      setup(setup0)
      () => ()
    }
}
object Requests {
  trait Setup[F[_]] {
    def request[K, V](source: DataSource[F, K, V], key: K): () => Option[V]
  }

  private final class MapKSetup[F[_], G[_]](setup: Setup[G], fk: F ~> G) extends Setup[F] {
    def request[K, V](source: DataSource[F, K, V], key: K): () => Option[V] =
      setup.request(source.mapK(fk), key)
  }

  private final class SourceState[F[_]](
      val index: Int,
      val ds: DataSource[F, Any, Any]
  ) {
    val keys: mutable.ArrayBuffer[Any] = mutable.ArrayBuffer.empty[Any]
    val keyIndex: mutable.HashMap[Any, Int] = mutable.HashMap.empty[Any, Int]

    def add(key: Any): Int =
      keyIndex.getOrElseUpdate(
        key, {
          val index = keys.length
          keys += key
          index
        }
      )
  }

  private final class RunSetup[F[_]] extends Setup[F] {
    val sourceIndex: mutable.HashMap[DSKey[?, ?], SourceState[F]] = mutable.HashMap.empty
    val sources: mutable.ArrayBuffer[SourceState[F]] = mutable.ArrayBuffer.empty
    var results: Array[DataSource.Result[Any, Any]] = _

    def request[K, V](source: DataSource[F, K, V], key: K): () => Option[V] = {
      val state = sourceIndex.getOrElseUpdate(
        source.key, {
          val index = sources.length
          val state = new SourceState(index, source.asInstanceOf[DataSource[F, Any, Any]])
          sources += state
          state
        }
      )
      val keyIndex = state.add(key)
      () => results(state.index).get(key, keyIndex).asInstanceOf[Option[V]]
    }
  }

  def fetch[F[_], K, V](source: DataSource[F, K, V], key: K): Requests[F, Option[V]] =
    Requests(_.request(source, key))

  def empty[F[_], A]: Requests[F, Unit] =
    Requests(_ => () => throw new NoSuchElementException("empty requests"))

  implicit def applicative[F[_]]: Applicative[Requests[F, *]] = new Applicative[Requests[F, *]] {
    def pure[A](x: A): Requests[F, A] =
      Requests(_ => () => x)

    def ap[A, B](ff: Requests[F, A => B])(fa: Requests[F, A]): Requests[F, B] =
      Requests { setup =>
        val f = ff.setup(setup)
        val a = fa.setup(setup)
        () => f()(a())
      }

    override def map[A, B](fa: Requests[F, A])(f: A => B): Requests[F, B] =
      Requests { setup =>
        val a = fa.setup(setup)
        () => f(a())
      }

    override def as[A, B](fa: Requests[F, A], b: B): Requests[F, B] =
      Requests { setup =>
        fa.setup(setup)
        () => b
      }

    override def void[A](fa: Requests[F, A]): Requests[F, Unit] =
      Requests { setup =>
        fa.setup(setup)
        () => ()
      }
  }

  private def runOne[F[_]: Applicative, A](rebuild: () => A, setup: RunSetup[F], state: SourceState[F]): F[A] =
    state.ds.batch(state.keys).map { result =>
      setup.results = Array(result.asInstanceOf[DataSource.Result[Any, Any]])
      rebuild()
    }

  private def runMany[F[_]: Parallel, A](
      rebuild: () => A,
      setup: RunSetup[F]
  )(implicit F: Applicative[F]): F[A] = {
    val fa = ArraySeq.from(setup.sources).parTraverse { source =>
      source.ds.batch(source.keys).tupleLeft(source.index)
    }

    fa.map { xs =>
      val results = new Array[DataSource.Result[Any, Any]](setup.sources.length)
      xs.foreach { case (index, result) =>
        results(index) = result.asInstanceOf[DataSource.Result[Any, Any]]
      }
      setup.results = results
      rebuild()
    }
  }

  def run[F[_]: Parallel, A](requests: Requests[F, A])(implicit F: Applicative[F]): F[A] = {
    val setup = new RunSetup[F]
    val rebuild = requests.setup(setup)
    setup.sources.length match {
      case 0 => F.pure(rebuild())
      case 1 => runOne(rebuild, setup, setup.sources(0))
      case _ => runMany(rebuild, setup)
    }
  }
}
