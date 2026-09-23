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

package hxl.natchez

import cats.data._
import cats._
import _root_.natchez._
import cats.effect._
import cats.implicits._
import hxl.natchez.TracedRunner
import hxl._
import munit.CatsEffectSuite
import scala.concurrent.duration._

class TracingTest extends CatsEffectSuite {
  case object SimpleKey extends DSKey[String, String]
  def simpleDataSource[F[_]](implicit F: Applicative[F]) = DataSource.from_(SimpleKey) { ks =>
    F.pure(ks.toList.map(s => s -> s).toMap)
  }

  test("parSubtrace preserves parallel lifted effects and result order") {
    implicit val trace: _root_.natchez.Trace[IO] = _root_.natchez.noop.NoopTrace[IO]()
    for {
      leftStarted <- Deferred[IO, Unit]
      rightStarted <- Deferred[IO, Unit]
      left = HxlT.parSubtrace("parallel")(
        Hxl.liftF(leftStarted.complete(()) *> rightStarted.get.as(1))
      )
      right = HxlT.parSubtrace("parallel")(
        Hxl.liftF(rightStarted.complete(()) *> leftStarted.get.as(2))
      )
      result <- TracedRunner.runPar((left, right).tupled).timeout(5.seconds)
    } yield assertEquals(result, (1, 2))
  }

  test("subtrace preserves sequential state and result order") {
    type Effect[A] = State[List[Int], A]
    implicit val trace: _root_.natchez.Trace[Effect] = _root_.natchez.noop.NoopTrace[Effect]()
    val left = HxlT.subtrace("sequential")(Hxl.liftF[Effect, Int](State.modify[List[Int]](_ :+ 1).as(1)))
    val right = HxlT.subtrace("sequential")(Hxl.liftF[Effect, Int](State.modify[List[Int]](_ :+ 2).as(2)))

    val result = TracedRunner.runSequential((left, right).tupled).run(Nil).value
    assertEquals(result, (List(1, 2), (1, 2)))
  }

  for (parallel <- List(false, true)) {
    val method = if (parallel) "parSubtrace" else "subtrace"

    test(s"$method preserves independent error scopes within one batch") {
      implicit val trace: _root_.natchez.Trace[IO] = _root_.natchez.noop.NoopTrace[IO]()
      for {
        batches <- Ref.of[IO, Vector[Set[Int]]](Vector.empty)
        source = DataSource.from_[IO, Int, Int](DSKey[Int, Int]) { keys =>
          batches.update(_ :+ keys.toSet).as(keys.iterator.map(key => key -> key).toMap)
        }
        program = Hxl.traverse(List(1, 2, 3)) { key =>
          Hxl.channel[IO, NonEmptyChain[String], Int] { raise =>
            val fa = Hxl.unsafeGet(key, source).andThen { value =>
              if (value < 3) raise.raise[Int](NonEmptyChain.one(s"error-$value"))
              else Hxl.pure[IO, Int](value)
            }
            if (parallel) HxlT.parSubtrace("scope")(fa) else HxlT.subtrace("scope")(fa)
          }
        }
        result <- TracedRunner.runPar(program)
        seen <- batches.get
      } yield {
        assertEquals(seen, Vector(Set(1, 2, 3)))
        assertEquals(result.toList, List(Left(NonEmptyChain.one("error-1")), Left(NonEmptyChain.one("error-2")), Right(3)))
      }
    }

    test(s"$method returns deferred errors to a shared outer channel") {
      implicit val trace: _root_.natchez.Trace[IO] = _root_.natchez.noop.NoopTrace[IO]()
      val program = Hxl.channel[IO, NonEmptyChain[String], List[Int]] { raise =>
        Hxl
          .traverse(List("first", "second")) { error =>
            val fa = Hxl.liftF(IO.unit).andThen(_ => raise.raise[Int](NonEmptyChain.one(error)))
            if (parallel) HxlT.parSubtrace("scope")(fa) else HxlT.subtrace("scope")(fa)
          }
          .map(_.toList)
      }

      TracedRunner.runPar(program).map(result => assertEquals(result, Left(NonEmptyChain("first", "second"))))
    }

    test(s"$method preserves inner channel precedence") {
      implicit val trace: _root_.natchez.Trace[IO] = _root_.natchez.noop.NoopTrace[IO]()
      val program = Hxl.channel[IO, NonEmptyChain[String], Either[NonEmptyChain[String], Int]] { _ =>
        val fa = Hxl.channel[IO, NonEmptyChain[String], Int] { inner =>
          Hxl.liftF(IO.unit).andThen(_ => inner.raise[Int](NonEmptyChain.one("inner")))
        }
        if (parallel) HxlT.parSubtrace("scope")(fa) else HxlT.subtrace("scope")(fa)
      }

      TracedRunner.runPar(program).map(result => assertEquals(result, Right(Left(NonEmptyChain.one("inner")))))
    }
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
