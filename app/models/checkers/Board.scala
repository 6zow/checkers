package models.checkers

import models.{User, Point}

/**
 * @author Max Gorbunov
 */
class Board {
  var pieces = {
    val seq = for {
      u <- 0 to 1
      p <- 0 to 11
    } yield (s"p${u + 1}-${p + 1}", (p / 3 * 2 + p % 3 % 2 + u) % 8 + 1, p % 3 + u % 2 * 5 + 1)
    seq.map {
      case (id, x, y) => id -> Point(x, y)
    }.toMap
  }
}

case class Piece(user: User, id: String, position: Point)