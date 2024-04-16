package u07.examples

import u07.utils.Time
import java.util.Random
import u07.examples.StochasticReadersWriters.*
import u07.modelling.CTMCSimulation.*
import scala.annotation.tailrec
import scala.annotation.internal.reachCapability

extension (self: Trace[MSet[Place]])
  @tailrec
  def timeSpentIn(
      state: MSet[Place],
      acc: Double = 0,
      times: Int = 0
  ): (Double, Int) =
    self match
      case Event(tstart, s) #:: Event(tend, _) #:: xs
          if !s.extract(state).isEmpty =>
        xs.timeSpentIn(state, acc + (tend - tstart), times + 1)
      case _ #:: xs => xs.timeSpentIn(state, acc, times)
      // Sometimes the event may never appear, so I want to avoid NaN
      case _ if times > 0 => (acc, times)
      case _              => (0, 1)

def doRun(
    num: Int,
    desc: String,
    t1Time: Double = 1,
    t2Time: Double = 200_000,
    t3Time: Double = 100_000,
    t4Time: Double = 100_000,
    t5Time: Double = 100_000,
    t6Time: Double = 0.1,
    t7Time: Double = 0.2
) =
  Time.timed:
    val runs = 100
    val simulations = (0 to runs).map(_ =>
      stocReadersWriters(
        t1Time,
        t2Time,
        t3Time,
        t4Time,
        t5Time,
        t6Time,
        t7Time
      )
        .newSimulationTrace(
          MSet.ofList(List.fill(10)(Idle) ++ List(Resource)),
          new Random
        )
        .take(100)
    )
    val writingAvg = simulations
      .map(sim =>
        val stats = sim.timeSpentIn(MSet(Writing))
        stats._1 / stats._2
      )
      .sum / runs
    val readingAvg = simulations
      .map(sim =>
        val stats = sim.timeSpentIn(MSet(Reading))
        stats._1 / stats._2
      )
      .sum / runs
    println:
      s"""
        Run $num $desc:
        reading: $readingAvg
        writing: $writingAvg
      """.trim()

@main def mainStochasticRWSimulation =
  doRun(1, "default values")
  doRun(2, "faster t6 transaction (10)", t6Time = 10)
  doRun(3, "faster t7 transaction (10)", t7Time = 10)
  doRun(
    4,
    "more and faster readers t2 1mil t6 10",
    t2Time = 1_000_000,
    t6Time = 10
  )
  doRun(
    5,
    "more and faster writers t3 1mil t7 10",
    t3Time = 1_000_000,
    t7Time = 10
  )
