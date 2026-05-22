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

package hxl.bench

import cats.effect.SyncIO
import cats.implicits._
import hxl._
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Fork(2)
@Threads(1)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@State(Scope.Benchmark)
class HxlTraverseBenchmark {
  import HxlTraverseBenchmark._

  @Param(Array("100", "1000", "10000"))
  var size: Int = _

  @Param(Array("1", "2", "3", "5", "10"))
  var depth: Int = _

  private var inputsList: List[Payload] = _
  private var inputsArray: Array[Payload] = _
  private var stages: Array[Int] = _
  private var dataSources: Array[DataSource[SyncIO, Payload, Payload]] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    inputsList = (0 until size).toList.map(i => Payload(i, Nil))
    inputsArray = inputsList.toArray
    stages = Array.tabulate(depth)(identity)
    dataSources = stages.map(stage => dataSource(stage))
    validate()
  }

  @Benchmark
  def catsTraverseHxl(blackhole: Blackhole): Unit =
    blackhole.consume(catsTraverseHxlResult().unsafeRunSync())

  @Benchmark
  def fastTraverseHxl(blackhole: Blackhole): Unit =
    blackhole.consume(fastTraverseHxlResult().unsafeRunSync())

  @Benchmark
  def manualMapBatch(blackhole: Blackhole): Unit =
    blackhole.consume(manualMapBatchResult().unsafeRunSync())

  @Benchmark
  def manualPairedBatch(blackhole: Blackhole): Unit =
    blackhole.consume(manualPairedBatchResult().unsafeRunSync())

  private def catsTraverseHxlResult(): SyncIO[List[Payload]] =
    Hxl.runSequential(inputsList.traverse(hxlProgram))

  private def fastTraverseHxlResult(): SyncIO[scala.collection.immutable.ArraySeq[Payload]] =
    Hxl.runSequential(Hxl.traverse(inputsArray)(hxlProgram))

  private def manualMapBatchResult(): SyncIO[List[Payload]] =
    runManual((stage, inputs) => mapBatch(stage, inputs).map(values => inputs.map(values)))

  private def manualPairedBatchResult(): SyncIO[List[Payload]] =
    runManual((stage, inputs) => pairedBatch(stage, inputs).map(_.map(_._2)))

  private def runManual(batch: (Int, List[Payload]) => SyncIO[List[Payload]]): SyncIO[List[Payload]] =
    stages.foldLeft(inputsList.pure[SyncIO]) { (result, stage) =>
      result.flatMap(batch(stage, _))
    }

  private def hxlProgram(input: Payload): Hxl[SyncIO, Payload] =
    dataSources.foldLeft(Hxl.pure[SyncIO, Payload](input)) { (result, ds) =>
      result.andThen { payload =>
        Hxl.unsafeGet(payload, ds)
      }
    }

  private def validate(): Unit = {
    val catsResult = catsTraverseHxlResult().unsafeRunSync()
    val fastResult = fastTraverseHxlResult().unsafeRunSync().toList
    val mapResult = manualMapBatchResult().unsafeRunSync()
    val pairedResult = manualPairedBatchResult().unsafeRunSync()

    if (catsResult != fastResult || catsResult != mapResult || catsResult != pairedResult) {
      throw new IllegalStateException("benchmark implementations produced different results")
    }
  }
}

object HxlTraverseBenchmark {
  final case class Payload(id: Int, path: List[Int])
  final case class StageKey(stage: Int) extends DSKey[Payload, Payload]

  private def dataSource(stage: Int): DataSource[SyncIO, Payload, Payload] =
    DataSource.full[SyncIO, Payload, Payload](StageKey(stage)) { keys =>
      SyncIO.pure(Map.from(keys.map(payload => payload -> step(stage, payload))))
    }

  private def mapBatch(stage: Int, inputs: List[Payload]): SyncIO[collection.Map[Payload, Payload]] =
    SyncIO.pure(Map.from(inputs.map(payload => payload -> step(stage, payload))))

  private def pairedBatch(stage: Int, inputs: List[Payload]): SyncIO[List[(Payload, Payload)]] =
    SyncIO.pure(inputs.map(payload => payload -> step(stage, payload)))

  private def step(stage: Int, payload: Payload): Payload =
    payload.copy(path = stage :: payload.path)
}
