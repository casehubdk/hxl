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
import scala.collection.immutable.ArraySeq

private[hxl] object HxlOpt {
  import Hxl._

  private def sequenceArray[F[_]](nodes: Array[Hxl[F, Any]])(implicit F: Applicative[F]): Hxl[F, Array[Any]] = {
    val active = new Array[Int](nodes.length)
    var i = 0
    while (i < nodes.length) {
      active(i) = i
      i += 1
    }
    sequenceActive(nodes, new Array[Any](nodes.length), active, active.length)
  }

  /*
   * Evaluates one layer of active slots. Operator priority is:
   * errors < lifts < binds < runs < done.
   *
   * Binds run in two phases: first evaluate every left side, while saving each
   * continuation with its original slot; then resume only those slots with the
   * corresponding left result. `safeFa` keeps deep left spines behind LiftF.
   */
  private def sequenceActive[F[_]](
      nodes: Array[Hxl[F, Any]],
      values: Array[Any],
      active: Array[Int],
      activeSize: Int
  )(implicit F: Applicative[F]): Hxl[F, Array[Any]] = {
    var errors = Chain.empty[Raised[?]]
    var lifts = 0
    var binds = 0
    var runs = 0
    var dones = 0
    var i = 0

    while (i < activeSize) {
      val index = active(i)
      nodes(index) match {
        case Errs(raiseds) =>
          errors = errors ++ raiseds.toChain
        case LiftF(_) =>
          lifts += 1
        case AndThen(_, _) =>
          binds += 1
        case Run(_) =>
          runs += 1
        case Done(value) =>
          values(index) = value
          dones += 1
      }
      i += 1
    }

    if (errors.nonEmpty) {
      Errs(NonEmptyChain.fromChainUnsafe(errors))
    } else if (lifts > 0) {
      val nextActiveSize = activeSize - dones
      val nextActive =
        if (dones == 0) active
        else {
          val indexes = new Array[Int](nextActiveSize)
          var j = 0
          i = 0
          while (i < activeSize) {
            val index = active(i)
            nodes(index) match {
              case Done(_) =>
                ()
              case _ =>
                indexes(j) = index
                j += 1
            }
            i += 1
          }
          indexes
        }

      val lifted = new Array[Any](lifts)
      val liftedIndexes = new Array[Int](lifts)
      var j = 0
      i = 0
      while (i < activeSize) {
        val index = active(i)
        nodes(index) match {
          case LiftF(fa) =>
            lifted(j) = fa
            liftedIndexes(j) = index
            j += 1
          case _ =>
            ()
        }
        i += 1
      }

      var effects = List.empty[F[Hxl[F, Any]]]
      j = lifts - 1
      while (j >= 0) {
        effects = lifted(j).asInstanceOf[F[Hxl[F, Any]]] :: effects
        j -= 1
      }

      LiftF {
        effects.sequence
          .map { liftedValues =>
            val next = nodes.clone()
            val nextValues = values.clone()
            val iterator = liftedValues.iterator
            var k = 0
            while (iterator.hasNext) {
              next(liftedIndexes(k)) = iterator.next()
              k += 1
            }
            sequenceActive(next, nextValues, nextActive, nextActiveSize)
          }
      }
    } else if (binds > 0) {
      val heads = nodes.clone()
      val continuations = new Array[Any => Hxl[F, Any]](binds)
      val bindIndexes = new Array[Int](binds)
      val headActiveSize = activeSize - dones
      val headActive =
        if (dones == 0) active
        else new Array[Int](headActiveSize)
      var headI = 0
      var bindI = 0
      i = 0
      while (i < activeSize) {
        val index = active(i)
        nodes(index) match {
          case andThen: AndThen[F, a, Any] =>
            heads(index) = andThen.safeFa.asInstanceOf[Hxl[F, Any]]
            continuations(bindI) = andThen.fb.asInstanceOf[Any => Hxl[F, Any]]
            bindIndexes(bindI) = index
            bindI += 1
            headActive(headI) = index
            headI += 1
          case Done(_) =>
            ()
          case _ =>
            headActive(headI) = index
            headI += 1
        }
        i += 1
      }

      sequenceActive(heads, values.clone(), headActive, headActiveSize).andThen { headValues =>
        val next = nodes.clone()
        val nextValues = headValues.clone()
        var k = 0
        while (k < binds) {
          val index = bindIndexes(k)
          next(index) = continuations(k)(headValues(index))
          k += 1
        }
        sequenceActive(next, nextValues, bindIndexes, binds)
      }
    } else if (runs > 0) {
      val runNodes = new Array[Requests[F, Any]](runs)
      val runIndexes = new Array[Int](runs)
      var j = 0
      i = 0
      while (i < activeSize) {
        val index = active(i)
        nodes(index) match {
          case Run(runRequests) =>
            runNodes(j) = runRequests
            runIndexes(j) = index
            j += 1
          case _ =>
            ()
        }
        i += 1
      }

      Run(
        Requests { setup =>
          val rebuilds = new Array[() => Any](runs)
          var k = 0
          while (k < runs) {
            rebuilds(k) = runNodes(k).setup(setup)
            k += 1
          }
          () => {
            val rebuilt = values.clone()
            k = 0
            while (k < runs) {
              rebuilt(runIndexes(k)) = rebuilds(k)()
              k += 1
            }
            rebuilt
          }
        }
      )
    } else {
      Done(values)
    }
  }

  private def resultArraySeq[A](values: Array[Any]): ArraySeq[A] =
    ArraySeq.unsafeWrapArray(values).asInstanceOf[ArraySeq[A]]

  private def emptyArraySeq[A]: ArraySeq[A] =
    ArraySeq.unsafeWrapArray(Array.empty[Any]).asInstanceOf[ArraySeq[A]]

  private def singleArraySeq[A](value: A): ArraySeq[A] =
    ArraySeq.unsafeWrapArray(Array[Any](value)).asInstanceOf[ArraySeq[A]]

  private def sequenceNodes[F[_]: Applicative, A](nodes: Array[Hxl[F, Any]]): Hxl[F, ArraySeq[A]] =
    nodes.length match {
      case 0 => Done(emptyArraySeq)
      case 1 => Hxl.fmap(nodes(0).asInstanceOf[Hxl[F, A]])(singleArraySeq)
      case _ => Hxl.fmap(sequenceArray(nodes))(resultArraySeq[A])
    }

  def sequence[F[_]: Applicative, A](xs: Array[Hxl[F, A]]): Hxl[F, ArraySeq[A]] = {
    val nodes = new Array[Hxl[F, Any]](xs.length)
    var i = 0
    while (i < nodes.length) {
      nodes(i) = xs(i).asInstanceOf[Hxl[F, Any]]
      i += 1
    }
    sequenceNodes(nodes)
  }

  def traverse[F[_]: Applicative, A, B](xs: Array[A])(f: A => Hxl[F, B]): Hxl[F, ArraySeq[B]] = {
    val nodes = new Array[Hxl[F, Any]](xs.length)
    var i = 0
    while (i < nodes.length) {
      nodes(i) = f(xs(i)).asInstanceOf[Hxl[F, Any]]
      i += 1
    }
    sequenceNodes(nodes)
  }
}
