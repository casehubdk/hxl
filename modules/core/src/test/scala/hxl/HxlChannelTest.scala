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
    DataSource.from_[Effect, Int, String](ChannelKey) { keys =>
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

  test("channel preserves liftF shape") {
    val program = Hxl.channel[Eval, Errors, Int] { _ =>
      Hxl.embedF(Eval.now(Hxl.pure[Eval, Int](1)))
    }

    assert(program.isInstanceOf[Hxl.LiftF[Eval, Either[Errors, Int]]])
    assertEquals(Hxl.runSequential(program).value, Right(1))
  }

  test("channel handles deep bind chains stack safely") {
    val program = Hxl.channel[Eval, Errors, Int] { raise =>
      (0 until 10000)
        .foldLeft(Hxl.pure[Eval, Int](0)) { (acc, _) =>
          acc.andThen(i => Hxl.pure[Eval, Int](i + 1))
        }
        .andThen(_ => raise.raise[Int](errors("boom")))
    }

    assertEquals(Hxl.runSequential(program).value, Left(errors("boom")))
  }

  test("explicitErrs preserves mixed error tags and skips failed continuations") {
    val first = Hxl.Raised(new Hxl.ErrorTag[String] {}, "first")
    val second = Hxl.Raised(new Hxl.ErrorTag[Int] {}, 2)
    val raised: NonEmptyChain[Hxl.Raised[?]] = NonEmptyChain(first, second)
    val program = Hxl.Errs[Id, Int](raised).andThen[Int](_ => fail("continuation must not run"))

    assertEquals(Hxl.runSequential(Hxl.explicitErrs(program)), Left(raised))
    assertEquals(Hxl.runSequential(Hxl.explicitErrs(Hxl.pure[Id, Int](1))), Right(1))
  }

  test("explicitErrs constructs deep strict-effect chains without running continuations") {
    var evaluated = 0
    val program = (0 until 10000).foldLeft(Hxl.pure[Id, Int](0)) { (acc, _) =>
      acc.andThen { i =>
        evaluated += 1
        Hxl.pure[Id, Int](i + 1)
      }
    }
    val captured = Hxl.explicitErrs(program)
    assertEquals(evaluated, 0)
    assertEquals(Hxl.runSequential(captured), Right(10000))
    assertEquals(evaluated, 10000)
  }

  test("explicitErrs handles errors in deep strict-effect chains") {
    val raised: NonEmptyChain[Hxl.Raised[?]] = NonEmptyChain.one(Hxl.Raised(new Hxl.ErrorTag[String] {}, "boom"))
    val program = (0 until 10000)
      .foldLeft(Hxl.pure[Id, Int](0))((acc, _) => acc.andThen(i => Hxl.pure[Id, Int](i + 1)))
      .andThen(_ => Hxl.Errs[Id, Int](raised))
      .andThen[Int](_ => fail("continuation must not run"))
    assertEquals(Hxl.runSequential(Hxl.explicitErrs(program)), Left(raised))
  }

  test("explicitErrs captures deferred errors through deep bind chains") {
    val raised: NonEmptyChain[Hxl.Raised[?]] = NonEmptyChain.one(Hxl.Raised(new Hxl.ErrorTag[String] {}, "boom"))
    var evaluated = false
    val program = Hxl.embedF(Eval.later {
      evaluated = true
      (0 until 10000)
        .foldLeft(Hxl.pure[Eval, Int](0))((acc, _) => acc.andThen(i => Hxl.pure[Eval, Int](i + 1)))
        .andThen(_ => Hxl.Errs[Eval, Int](raised))
    })
    val captured = Hxl.explicitErrs(program)

    assert(!evaluated)
    assertEquals(Hxl.runSequential(captured).value, Left(raised))
    assert(evaluated)
  }

  test("explicitErrs preserves request batching and per-request outcomes") {
    val raised: NonEmptyChain[Hxl.Raised[?]] = NonEmptyChain.one(Hxl.Raised(new Hxl.ErrorTag[String] {}, "odd"))
    val program = Hxl.traverse(List(1, 2)) { key =>
      Hxl.explicitErrs(Hxl(key, loggingDataSource).andThen { value =>
        if (key % 2 == 0) Hxl.pure[Effect, Option[String]](value)
        else Hxl.Errs[Effect, Option[String]](raised)
      })
    }
    val (log, result) = Hxl.runSequential(program).run(Vector.empty)

    assertEquals(log, Vector(Set(1, 2)))
    assertEquals(result.toList, List(Left(raised), Right(Some("value-2"))))
  }

  test("explicitErrs respects inner channels and allows re-emission to outer channels") {
    val inner = Hxl.channel[Id, Errors, Int] { raise =>
      Hxl.pure[Id, Unit](()).andThen(_ => raise.raise[Int](errors("inner")))
    }
    assertEquals(Hxl.runSequential(Hxl.explicitErrs(inner)), Right(Left(errors("inner"))))

    val outer = Hxl.channel[Id, Errors, Int] { raise =>
      val captured = Hxl.runSequential(Hxl.explicitErrs(raise.raise[Int](errors("outer"))))
      Hxl.pure[Id, Unit](()).andThen { _ =>
        captured match {
          case Left(es) => Hxl.Errs[Id, Int](es)
          case Right(a) => Hxl.pure[Id, Int](a)
        }
      }
    }
    assertEquals(Hxl.runSequential(outer), Left(errors("outer")))
  }

  test("explicitErrs preserves channel batching across deep merged branches") {
    for ((leftDepth, rightDepth) <- List((31, 32), (32, 33), (33, 34), (64, 33), (65, 66), (100, 64))) {
      val left = (0 until leftDepth).foldLeft(Hxl(0, loggingDataSource)) { (acc, key) =>
        acc.andThen(_ => Hxl(key + 1, loggingDataSource))
      }
      val right = (0 until rightDepth).foldLeft(Hxl(1000, loggingDataSource)) { (acc, key) =>
        acc.andThen(_ => Hxl(key + 1001, loggingDataSource))
      }
      val captured = (Hxl.explicitErrs(left), Hxl.explicitErrs(right)).tupled
      val handled = (
        Hxl.channel[Effect, Errors, Option[String]](_ => left),
        Hxl.channel[Effect, Errors, Option[String]](_ => right)
      ).tupled

      val (capturedLog, capturedResult) = Hxl.runSequential(captured).run(Vector.empty)
      val (handledLog, handledResult) = Hxl.runSequential(handled).run(Vector.empty)
      assertEquals(capturedLog, handledLog)
      assertEquals(
        (capturedResult._1.toOption, capturedResult._2.toOption),
        (handledResult._1.toOption, handledResult._2.toOption)
      )
    }
  }
}
