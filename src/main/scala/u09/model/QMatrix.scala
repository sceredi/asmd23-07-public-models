package u09.model

import java.awt.SystemTray

object QMatrix:

  type Node = (Int, Int)

  enum Move:
    case LEFT, RIGHT, UP, DOWN
    override def toString =
      Map(LEFT -> "<", RIGHT -> ">", UP -> "^", DOWN -> "v")(this)

  import Move.*

  case class Facade(
      width: Int,
      height: Int,
      initial: Node,
      terminal: PartialFunction[Node, Boolean],
      reward: PartialFunction[(Node, Move), Double],
      jumps: PartialFunction[(Node, Move), Node],
      obstacles: List[Node],
      gamma: Double,
      alpha: Double,
      epsilon: Double = 0.0,
      v0: Double
  ) extends QRLImpl:
    type State = Node
    type Action = Move

    def qEnvironment(): Environment = (s: Node, a: Move) =>
      // applies direction, without escaping borders
      val newNode: Node = (s, a) match
        case ((n1, n2), UP)    => (n1, (n2 - 1) max 0)
        case ((n1, n2), DOWN)  => (n1, (n2 + 1) min (height - 1))
        case ((n1, n2), LEFT)  => ((n1 - 1) max 0, n2)
        case ((n1, n2), RIGHT) => ((n1 + 1) min (width - 1), n2)
        case _                 => ???
      // computes obstacles
      val n2 = if obstacles.contains(newNode) then s else newNode
      // computes rewards, and possibly a jump
      (reward.apply((s, a)), jumps.orElse[(Node, Move), Node](_ => n2)(s, a))

    def qFunction = QFunction(Move.values.toSet, v0, terminal)
    def qSystem = QSystem(environment = qEnvironment(), initial, terminal)
    def makeLearningInstance() =
      QLearning(qSystem, gamma, alpha, epsilon, qFunction)

    def show[E](
        v: Node => E,
        formatNode: String,
        formatString: String
    ): String =
      (for
        row <- 0 until width
        col <- 0 until height
      yield printNode((row, col), v, formatNode, formatString) +
        (if (col == height - 1) "\n"
         else "\t"))
        .mkString("")

    private def printNode[E](
        node: Node,
        v: Node => E,
        formatNode: String,
        formatString: String
    ): String =
      node match
        case node if obstacles.contains(node) => formatString.format("#")
        case node if node == initial          => formatString.format("s")
        case node
            if reward((node, UP)) + reward((node, DOWN)) +
              reward((node, LEFT)) + reward((node, RIGHT)) > 0 =>
          formatString.format("w")
        case (row, col) => formatNode.format(v(row, col))
