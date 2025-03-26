/*
 * Copyright 2025 CaseHubDK
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

package hxl.natchez

import cats.data._
import cats._
import _root_.natchez._
import cats.effect._
import cats.implicits._
import hxl.natchez.TracedRunner
import hxl._
import munit.CatsEffectSuite

class TracingTest extends CatsEffectSuite {
  case object SimpleKey extends DSKey[String, String]
  def simpleDataSource[F[_]](implicit F: Applicative[F]) = DataSource.from(SimpleKey) { ks =>
    F.pure(ks.toList.map(s => s -> s).toMap)
  }

  test("should trace requests and add rounds") {
    type Effect[A] = Kleisli[IO, Span[IO], A]
    val fa = Hxl("foo", simpleDataSource[Effect])
    val fb = (fa, fa).mapN(_.mkString + " " + _.mkString)
    val fc = fb.andThen(_ => fa)

    InMemory.EntryPoint.create[IO].flatMap { ep =>
      ep
        .root("test-root")
        .useKleisli(TracedRunner.runSequential(fc)) >> ep.ref.get.map { data =>
        _root_.natchez.TraceValue
        val x = data.foldMap {
          case ((_, InMemory.NatchezCommand.Put(List(("round", TraceValue.NumberValue(n)))))) =>
            n.intValue()
          case _ => 0
        }
        assertEquals(x, 3)
      }
    }
  }
}
