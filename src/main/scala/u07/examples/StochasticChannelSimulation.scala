package u07.examples

import u07.utils.Time
import java.util.Random
import u07.examples.StochasticChannel.*

@main def mainStochasticChannelSimulation =
  Time.timed:
    println:
      stocChannel
        .newSimulationTrace(IDLE, new Random)
        .take(10)
        .toList
        .mkString("\n")

@main def avgTimeToDoneCommunication =
  Time.timed:
    val runs = 100_000
    val simulations = (1 to runs).map(i =>
      stocChannel
        .newSimulationTrace(IDLE, new Random)
    )
    val avgTilDone = simulations.map(_.find(_._2 == DONE).get._1).sum / runs
    println(
      s"avg time until done communication $avgTilDone"
    )
    def amountSpentInFail(l: LazyList[Event[State]]): Double =
      l match
        case Event(tsart, FAIL) #:: Event(tend, _) #:: xs =>
          tend - tsart + amountSpentInFail(xs)
        case Event(_, DONE) #:: _ => 0
        case _ #:: xs             => amountSpentInFail(xs)
        case _                    => 0
    println(
      s"avg time spent in fail ${simulations.map(amountSpentInFail(_)).sum / runs}"
    )

