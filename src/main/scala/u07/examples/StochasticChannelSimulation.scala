package u07.examples

import u07.utils.Time
import java.util.Random
import u07.examples.StochasticChannel.*
import u07.modelling.CTMCSimulation.*

extension [A](self: Trace[A])
  def timeUntil(state: A): Option[Double] =
    self find (_._2 == state) map (_._1)

  def timeSpentIn(state: A): Double =
    self match
      case Event(tsart, `state`) #:: Event(tend, _) #:: xs =>
        tend - tsart + xs.timeSpentIn(state)
      case Event(_, DONE) #:: _ => 0
      case _ #:: xs             => xs.timeSpentIn(state)
      case _                    => 0

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
    val avgTilDone = simulations.map(_.timeUntil(DONE).get).sum / runs
    println(
      s"avg time until done communication $avgTilDone"
    )
    println(
      s"avg time spent in fail ${simulations.map(_.timeSpentIn(FAIL)).sum / runs}"
    )
