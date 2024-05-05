package u09.examples

import u09.model.QMatrix

object TryQMatrix extends App:

  import u09.model.QMatrix.Move.*
  import u09.model.QMatrix.*

  val height = 20
  val heightDecreased = height - 1
  val rl: QMatrix.Facade = Facade(
    width = 2,
    height = height,
    initial = (0, 0),
    terminal = { case _ => false },
    reward = {
      case ((0, `heightDecreased`), _) => 10; case _ => 0
    },
    jumps = { case ((`height`, `height`), _) => (0, 0) },
    obstacles = (0 until height)
      .filter(_ % 2 == 0)
      .map(i => (if i % 4 == 0 then 1 else 0, i))
      .toList,
    gamma = 0.9,
    alpha = 0.5,
    epsilon = 0.3,
    v0 = 1
  )

  val q0 = rl.qFunction
  println(rl.show(q0.vFunction, "%2.2f", "%4s"))
  val q1 = rl.makeLearningInstance().learn(10000, 100, q0)
  println(rl.show(q1.vFunction, "%2.2f", "%4s"))
  println(rl.show(s => q1.bestPolicy(s).toString, "%7s", "%7s"))
