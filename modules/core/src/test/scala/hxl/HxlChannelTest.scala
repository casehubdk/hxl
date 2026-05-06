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
import munit.FunSuite

class HxlChannelTest extends FunSuite {
  type Errors = NonEmptyChain[String]
  type Log = Vector[Set[Int]]
  type Effect[A] = StateT[Id, Log, A]

  case object ChannelKey extends DSKey[Int, String]

  def errors(value: String): Errors =
    NonEmptyChain.one(value)

  def errorSet[A](result: Either[Errors, A]): Either[Set[String], A] =
    result.leftMap(_.toChain.toList.toSet)

  def loggingDataSource: DataSource[Effect, Int, String] =
    DataSource.from[Effect, Int, String](ChannelKey) { keys =>
      StateT[Id, Log, Map[Int, String]] { log =>
        val values = keys.toList.map(k => k -> s"value-$k").toMap
        (log :+ keys.toList.toSet, values)
      }
    }

  def fetchOrRaise(x: Int, raise: Hxl.Raise[Effect, Errors]): Hxl[Effect, String] =
    Hxl(x, loggingDataSource).andThen {
      case Some(value) if x % 2 == 0 => Hxl.pure[Effect, String](value)
      case Some(_)                   => raise.raise[String](errors(s"odd-$x"))
      case None                      => raise.raise[String](errors(s"missing-$x"))
    }

  test("channel placement controls error scope without changing batching") {
    val xs = List(1, 2, 3, 4)

    val perItem = xs.traverse { x =>
      Hxl.channel[Effect, Errors, String](raise => fetchOrRaise(x, raise))
    }
    val whole = Hxl.channel[Effect, Errors, List[String]] { raise =>
      xs.traverse(x => fetchOrRaise(x, raise))
    }

    val (perItemLog, perItemResult) = Hxl.runSequential(perItem).run(Vector.empty)
    val (wholeLog, wholeResult) = Hxl.runSequential(whole).run(Vector.empty)

    assertEquals(perItemLog, Vector(Set(1, 2, 3, 4)))
    assertEquals(wholeLog, Vector(Set(1, 2, 3, 4)))
    assertEquals(
      perItemResult,
      List(
        Left(errors("odd-1")),
        Right("value-2"),
        Left(errors("odd-3")),
        Right("value-4")
      )
    )
    assertEquals(errorSet(wholeResult), Left(Set("odd-1", "odd-3")))
  }

  test("applicative errors accumulate inside one channel") {
    val program = Hxl.channel[Id, Errors, (Unit, Unit)] { raise =>
      (
        raise.raise[Unit](errors("a")),
        raise.raise[Unit](errors("b"))
      ).tupled
    }

    val result = Hxl.runSequential[Id, Either[Errors, (Unit, Unit)]](program)

    assertEquals(errorSet(result), Left(Set("a", "b")))
  }

  test("inner channel wins when inner and outer errors coexist") {
    val program = Hxl.channel[Id, Errors, Either[Errors, Unit]] { outer =>
      Hxl.channel[Id, Errors, Unit] { inner =>
        (
          outer.raise[Unit](errors("outer")),
          inner.raise[Unit](errors("inner"))
        ).tupled.void
      }
    }

    assertEquals(Hxl.runSequential[Id, Either[Errors, Either[Errors, Unit]]](program), Right(Left(errors("inner"))))
  }

  test("handled errors compose with normal hxl code") {
    val program = Hxl
      .channel[Id, Errors, String](raise => raise.raise[String](errors("boom")))
      .map {
        case Left(es)     => s"recovered:${es.toChain.toList.mkString(",")}"
        case Right(value) => value
      }

    assertEquals(Hxl.runSequential[Id, String](program), "recovered:boom")
  }
}
