package u07.examples

import u07.modelling.SPN
import u07.utils.MSet

object StochasticReadersWriters:
  enum Place:
    case Idle, Entering, WaitingRead, WaitingWrite, Resource, Reading, Writing

  export Place.*
  export u07.modelling.SPN.*
  export u07.utils.MSet

  def stocReadersWriters(
      t1Time: Double = 1,
      t2Time: Double = 200_000,
      t3Time: Double = 100_000,
      t4Time: Double = 100_000,
      t5Time: Double = 100_000,
      t6Time: Double = 0.1,
      t7Time: Double = 0.2
  ) =
    toCTMC(
      SPN[Place](
        Trn(MSet(Idle), _ => t1Time, MSet(Entering), MSet()),
        Trn(MSet(Entering), _ => t2Time, MSet(WaitingRead), MSet()),
        Trn(MSet(Entering), _ => t3Time, MSet(WaitingWrite), MSet()),
        Trn(
          MSet(WaitingRead, Resource),
          _ => t4Time,
          MSet(Resource, Reading),
          MSet()
        ),
        Trn(
          MSet(WaitingWrite, Resource),
          _ => t5Time,
          MSet(Writing),
          MSet(Reading)
        ),
        Trn(MSet(Reading), _.size * t6Time, MSet(Idle), MSet()),
        Trn(MSet(Writing), _ => t7Time, MSet(Idle, Resource), MSet())
      )
    )

@main def mainStocReadersWriters() =
  import StochasticReadersWriters.*
  Place.values.foreach(s =>
    println(s"$s,${stocReadersWriters().transitions(MSet(s))}")
  )
