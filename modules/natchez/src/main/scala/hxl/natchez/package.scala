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

import _root_.natchez._
import cats.data._
import cats.implicits._
import cats._
import Hxl._

package object `natchez` {
  import NatchezInternal._
  object HxlT {
    def parSubtrace[F[_]: Monad: Parallel: Trace, A](name: String)(fa: Hxl[F, A]): Hxl[F, A] = {
      val ds = DataSource.from[F, HxlSpanKey[F, A], A](HxlSpanDSKey[F, A](name)) { keys =>
        val h = keys.toList.traverse(k => k.fa.map(a => (k, a))).map(_.toMap)
        Trace[F].span(s"subbatch-hxl-$name") {
          TracedRunner.runPar(h)
        }
      }
      Hxl(
        new HxlSpanKey(fa),
        ds
      ).map(_.get)
    }
    def subtrace[F[_]: Monad: Trace, A](name: String)(fa: Hxl[F, A]): Hxl[F, A] = {
      implicit val P: Parallel[F] = Parallel.identity[F]
      parSubtrace[F, A](name)(fa)
    }
  }

  object TracedRunner {
    def runPar[F[_]: Parallel: Monad: Trace, A](fa: Hxl[F, A]): F[A] =
      fa.foldMap(composeTracing[F, F](Hxl.parallelRunner)).runA(1)

    def runSequential[F[_]: Monad: Trace, A](fa: Hxl[F, A]): F[A] = {
      implicit val P: Parallel[F] = Parallel.identity[F]
      runPar[F, A](fa)
    }
  }
}

object NatchezInternal {
  final class HxlSpanKey[F[_], A](val fa: Hxl[F, A])
  final case class HxlSpanDSKey[F[_], A](name: String) extends DSKey[HxlSpanKey[F, A], A]

  def traceRequests[F[_]: Trace: Applicative, A](req: Requests[F, A]): Requests[F, A] = {
    def traceSource[K, V](source: DataSource[F, K, V]): DataSource[F, K, V] =
      DataSource.full[F, K, source.K2, V](source.key)(source.k2) { ks =>
        Trace[F].span(s"datasource.${source.key}") {
          Trace[F].put("keys" -> ks.size) *> source.batch(ks)
        }
      }(source.optimization)

    req.visit {
      new Requests.DataSourceVisitor[F] {
        def visit[K, V](source: DataSource[F, K, V], k: K): (DataSource[F, K, V], K) =
          (traceSource(source), k)
      }
    }
  }

  def composeTracing[F[_]: Trace: Applicative, G[_]: Trace: Applicative](
      compiler: Compiler[F, G]
  ): Compiler[F, StateT[G, Int, *]] = {
    type Effect[A] = StateT[G, Int, A]
    new Compiler[F, Effect] {
      def apply[A](fa: NonBind[F, A]): Hxl.Target[F, Effect, A] =
        fa match {
          case run: Hxl.Run[F, A] =>
            StateT { (round: Int) =>
              Trace[G]
                .span("hxl.run") {
                  Trace[G].put("round" -> round) *> compiler {
                    Hxl.Run(traceRequests(run.requests))
                  }
                }
                .map(round + 1 -> _)
            }
          case other => StateT.liftF(compiler(other))
        }
    }
  }

}
