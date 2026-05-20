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

import munit.FunSuite
import cats.data._
import cats._
import cats.implicits._
import scala.collection.concurrent.TrieMap

final case class FailingKey(key: String) extends DSKey[String, String]

class HxlEvaluationTest extends FunSuite {
  case object SimpleKey extends DSKey[String, String]
  def simpleDataSource[F[_]](implicit F: Applicative[F]) = DataSource.from(SimpleKey) { ks =>
    F.pure(ks.toList.map(s => s -> s))
  }

  test("should be able to construct and evaluate a hxl in Id") {
    val fa = Hxl("foo", simpleDataSource[Id])
    val result = (fa, fa).mapN(_.mkString + " " + _.mkString)
    assertEquals(Hxl.runSequential(result), "foo foo")
  }

  test("should be able to conduct the same test but in Eval instead") {
    val fa = Hxl("foo", simpleDataSource[Eval])
    val result = (fa, fa).mapN(_.mkString + " " + _.mkString)
    assertEquals(Hxl.runSequential(result).value, "foo foo")
  }

  case object SimpleKey2 extends DSKey[String, String]
  def simpleDataSource2[F[_]](implicit F: Applicative[F]) = DataSource.from(SimpleKey2) { ks =>
    F.pure(ks.toList.map(s => s -> s))
  }

  test(s"should be able to mix two different data sources together") {
    val fa = Hxl("foo", simpleDataSource[Id])
    val fb = Hxl("bar", simpleDataSource2[Id])
    val result = (fa, fb).mapN(_.mkString + " " + _.mkString)
    assertEquals(Hxl.runSequential(result), "foo bar")
  }

  test(s"monadic composition also works") {
    val fa = Hxl("foo", simpleDataSource[Id])
    val fb = Hxl("bar", simpleDataSource2[Id])
    val result = fa.andThen(a => fb.map(b => a.mkString + " " + b.mkString))
    assertEquals(Hxl.runSequential(result), "foo bar")
  }

  case object StatefulKey extends DSKey[String, String]
  def statefulDataSource[F[_]](implicit F: Applicative[F]) = DataSource.from(StatefulKey) { ks =>
    StateT { (i: Int) =>
      F.pure {
        (i + ks.size, ks.toList.map(s => s -> s).toMap)
      }
    }
  }

  test(s"should de-duplicate same keys") {
    val fa = Hxl("foo", statefulDataSource[Id])
    val n = Hxl.runSequential((fa, fa).tupled.void).runS(0)
    assertEquals(n, 1)
  }

  test(s"should not de-duplicate between rounds") {
    val fa = Hxl("foo", statefulDataSource[Id])
    val n = Hxl.runSequential((fa.monadic >> fa.monadic).hxl).runS(0)
    assertEquals(n, 2)
  }

  val lift: Id ~> Eval = new (Id ~> Eval) {
    def apply[A](a: Id[A]): Eval[A] = Eval.now(a)
  }

  test(s"can translate the effect type") {
    val fa = Hxl("foo", simpleDataSource[Id])
    val result = fa.mapK(lift)
    assertEquals(Hxl.runSequential(result).value, Some("foo"))
  }

  test(s"can translate the datasource") {
    val fa = Hxl("foo", simpleDataSource[Id].mapK(lift))
    assertEquals(Hxl.runSequential(fa).value, Some("foo"))
  }

  test("round scheduling for liftF") {
    val fa = Hxl("foo", statefulDataSource[Eval])
    val result = Hxl.runSequential((Hxl.embedF(fa.pure[StateT[Eval, Int, *]]), fa).tupled)
    assertEquals(result.runS(0).value, 1)
  }

  test("Hxl.sequence matches cats sequence") {
    val xs = List("foo", "bar", "foo").map(Hxl(_, simpleDataSource[Id]))
    assertEquals(Hxl.runSequential(Hxl.sequence(xs.toArray)).toList, Hxl.runSequential(xs.sequence))
  }

  test("Hxl.sequence batches runs and preserves done slots") {
    type Effect[A] = StateT[Id, Int, A]
    val fa = Hxl("foo", statefulDataSource[Id])
    val done = Hxl.pure[Effect, Option[String]](Some("done"))
    val result = Hxl.runSequential(Hxl.sequence(Array(fa, done, fa)))
    val (state, values) = result.run(0)
    assertEquals((state, values.toList), (1, List(Some("foo"), Some("done"), Some("foo"))))
  }

  test("Hxl.sequence preserves liftF round scheduling") {
    val fa = Hxl("foo", statefulDataSource[Eval])
    val lifted = Hxl.embedF(fa.pure[StateT[Eval, Int, *]])
    val result = Hxl.runSequential(Hxl.sequence(Array(lifted, fa)))
    assertEquals(result.runS(0).value, 1)
  }

  test("Hxl.sequence keeps andThen continuations paired with their lhs") {
    val left = Hxl.pure[Id, Int](1).andThen(i => Hxl.pure[Id, String]((i + 1).toString))
    val right = Hxl.pure[Id, String]("a").andThen(s => Hxl.pure[Id, String](s + "b"))
    assertEquals(Hxl.runSequential(Hxl.sequence(Array(left, right))).toList, List("2", "ab"))
  }

  test("deep applicative composition of binds is stack safe") {
    val values = (0 until 10000).toList
    def node(i: Int): Hxl[Id, Int] =
      Hxl.pure[Id, Int](i).andThen(i => Hxl.pure[Id, Int](i + 1))

    val result = Hxl.runSequential(values.traverse(node))
    val fastResult = Hxl.runSequential(Hxl.traverse(values.toArray)(node))
    assertEquals(result.headOption, Some(1))
    assertEquals(result.lastOption, Some(10000))
    assertEquals(fastResult.headOption, Some(1))
    assertEquals(fastResult.lastOption, Some(10000))
  }

  def failingDataSource[F[_]](key: String)(implicit F: ApplicativeError[F, NonEmptyChain[String]]) =
    DataSource.from(FailingKey(key)) { _ =>
      F.raiseError[Map[String, String]](NonEmptyChain.one("error"))
    }

  import hxl.instances.parallel._
  test("parallel composition in runner") {
    type Effect[A] = EitherNec[String, A]
    val fa = Hxl("foo", failingDataSource[Effect]("foo"))
    val fb = Hxl("bar", failingDataSource[Effect]("bar"))
    val errs = Hxl.runPar((fa, fb).tupled).void.left.toOption.foldMap(_.size)
    assertEquals(errs, 2L)
  }

  test("sequential composition in runner") {
    type Effect[A] = EitherNec[String, A]
    val fa = Hxl("foo", failingDataSource[Effect]("foo"))
    val fb = Hxl("bar", failingDataSource[Effect]("bar"))
    val errs = Hxl.runSequential((fa, fb).tupled).void.left.toOption.foldMap(_.size)
    assertEquals(errs, 1L)
  }

  test(s"parallel composition for hxl (liftF in particular)") {
    val fa = Hxl.liftF[EitherNec[String, *], Unit](Left(NonEmptyChain.one("error")))
    val errs = Hxl.runSequential((fa, fa).parTupled).left.toOption.foldMap(_.size)
    assertEquals(errs, 2L)
  }

  test(s"sequential composition for hxl (liftF in particular)") {
    val fa = Hxl.liftF[EitherNec[String, *], Unit](Left(NonEmptyChain.one("error")))
    val errs = Hxl.runSequential((fa, fa).tupled).left.toOption.foldMap(_.size)
    assertEquals(errs, 1L)
  }

  test("Hxl.sequence accumulates visible hxl errors") {
    val result = Hxl.channel[Id, String, scala.collection.immutable.ArraySeq[Unit]] { raise =>
      Hxl.sequence(Array(raise.raise[Unit]("a"), raise.raise[Unit]("b")))
    }
    assertEquals(Hxl.runSequential(result), Left("ab"))
  }

  test("benchmark nondiscards") {
    val rng = (0 until 1000).toList
    def s = rng.traverse(_ => Hxl("foo", simpleDataSource[Id]).void)
    def fast = Hxl.traverse(rng.toArray)(_ => Hxl("foo", simpleDataSource[Id]).void)
    // warmup
    (0 until 100).foreach { _ =>
      Hxl.runSequential(s)
      Hxl.runSequential(fast)
    }
    // report
    val start = System.currentTimeMillis()
    Hxl.runSequential(s)
    val mid = System.currentTimeMillis()
    Hxl.runSequential(fast)
    val end = System.currentTimeMillis()
    println("Benchmark result: cats=" + (mid - start) + "ms fast=" + (end - mid) + "ms")
  }

  test("benchmark bind-heavy traverse") {
    val rng = (0 until 1000).toList
    def node = Hxl.pure[Id, Unit](()).andThen(_ => Hxl("foo", simpleDataSource[Id]).void)
    def s = rng.traverse(_ => node)
    def fast = Hxl.traverse(rng.toArray)(_ => node)
    // warmup
    (0 until 100).foreach { _ =>
      Hxl.runSequential(s)
      Hxl.runSequential(fast)
    }
    // report
    val start = System.currentTimeMillis()
    Hxl.runSequential(s)
    val mid = System.currentTimeMillis()
    Hxl.runSequential(fast)
    val end = System.currentTimeMillis()
    println("Bind benchmark result: cats=" + (mid - start) + "ms fast=" + (end - mid) + "ms")
  }

  test("benchmark discards") {
    val rng = (0 until 1000).toList
    def s = rng.traverse(_ => Hxl.discard("foo", simpleDataSource[Id]))
    // warmup
    (0 until 100).foreach(_ => Hxl.runSequential(s))
    // report
    val start = System.currentTimeMillis()
    Hxl.runSequential(s)
    val end = System.currentTimeMillis()
    println("Benchmark result: " + (end - start) + "ms")
  }

  test("discard still runs and de-duplicates requests") {
    val fa = Hxl.discard("foo", statefulDataSource[Id])
    val n = Hxl.runSequential(Hxl.sequence(Array(fa, fa)).void).runS(0)
    assertEquals(n, 1)
  }

  case object AssocKey extends DSKey[Int, String]
  test("assoc datasource uses indexed results") {
    var seen = Vector.empty[List[Int]]
    val ds = DataSource.assoc[Id, Int, String](AssocKey) { keys =>
      seen = seen :+ keys.toList
      keys.take(2).map("value-" + _)
    }

    val result = Hxl.runSequential(Hxl.traverse(Array(1, 2, 1, 3))(Hxl(_, ds)))
    assertEquals(result.toList, List(Some("value-1"), Some("value-2"), Some("value-1"), None))
    assertEquals(seen, Vector(List(1, 2, 3)))
  }

  case class VarKey(str: String) extends DSKey[String, String]
  test("alignment") {
    val m = TrieMap.empty[String, Int]
    def ds(s: String) = DataSource.from(VarKey(s)) { ks =>
      m.updateWith(s) {
        case None    => Some(1)
        case Some(i) => Some(i + 1)
      }
      ks.toList.map(s => s -> s).toMap.pure[Id]
    }
    val suf = Hxl("b", ds("b"))

    def f(
        x: Option[String],
        align: Boolean
    ): Hxl[Id, Unit] = {
      def doAlign[A](fa: Hxl[Id, A]): Hxl[Id, A] =
        if (align) fa
        else fa

      for {
        _ <- doAlign {
          x.traverse(str => Hxl(str, ds("a")))
            .map(_.flatten)
            .andThen(_.traverse { str2 =>
              doAlign(Hxl(str2, ds("a2")))
            })
        }.monadic
        // _ <- doAlign(Hxl("a", ds("a"))).monadic
        _ <- doAlign(suf).monadic
      } yield ()
    }.hxl

    val p1 = f(None, true)
    val p2 = f("a".some, true)
    val p = p1 *> p2
    Hxl.runSequential(p)
    assertEquals(m("a"), 1)
    assertEquals(m("b"), 1)
  }

  test("Hxl.sequence preserves bind alignment across optional branches") {
    val m = TrieMap.empty[String, Int]
    def ds(s: String) = DataSource.from(VarKey(s)) { ks =>
      m.updateWith(s) {
        case None    => Some(1)
        case Some(i) => Some(i + 1)
      }
      ks.toList.map(s => s -> s).toMap.pure[Id]
    }
    val suf = Hxl("b", ds("b"))

    def f(x: Option[String]): Hxl[Id, Unit] =
      (
        x.traverse(str => Hxl(str, ds("a")))
          .map(_.flatten)
          .andThen(_.traverse(str2 => Hxl(str2, ds("a2"))))
          .void,
        suf
      ).tupled.void

    Hxl.runSequential(Hxl.sequence(Array(f(None), f("a".some))).void)
    assertEquals(m("a"), 1)
    assertEquals(m("a2"), 1)
    assertEquals(m("b"), 1)
  }
}
