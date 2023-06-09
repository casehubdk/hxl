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

import _root_.natchez._
import cats.data._
import cats.implicits._
import cats._
import _root_.hxl.Hxl._

package object `natchez` {
  def traceRequests[F[_]: Trace: Applicative, A](req: Requests[F, A]): Requests[F, A] = req match {
    case Requests.Pure(value)     => Requests.Pure(value)
    case ap: Requests.Ap[F, a, b] => Requests.Ap(traceRequests(ap.left), traceRequests(ap.right))
    case lift: Requests.Lift[F, k, a] =>
      val newSource = DataSource.full[F, k, lift.source.K2, a](lift.source.key)(lift.source.getKey) { ks =>
        Trace[F].span(s"datasource.${lift.source.key}") {
          Trace[F].put("keys" -> ks.size.toString) *> lift.source.batch(ks)
        }
      }

      Requests.Lift(newSource, lift.key)
  }

  def composeTracing[F[_]: Trace: Applicative, G[_]: Trace: Applicative](
      compiler: Compiler[F, G]
  ): Compiler[F, StateT[G, Int, *]] = {
    type Effect[A] = StateT[G, Int, A]
    new Compiler[F, Effect] {
      def apply[A](fa: Hxl[F, A]): Hxl.Target[F, Effect, A] =
        fa match {
          case Hxl.LiftF(unFetch) =>
            StateT.liftF {
              Trace[G].span("hxl.fetch") {
                compiler(Hxl.LiftF(unFetch))
              }
            }
          case bind: Hxl.Bind[F, a, b] =>
            StateT { round: Int =>
              Trace[G]
                .span("hxl.bind") {
                  Trace[G].put("round" -> round.toString) *> compiler {
                    Hxl.Bind(traceRequests(bind.requests), bind.f)
                  }
                }
                .map(round + 1 -> _)
            }
          case other => StateT.liftF(compiler(other))
        }
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
