package models.checkers

import models.{User, Point}

/**
 * @author Max Gorbunov
 */
class Board(val users: List[User]) {
  def update(piece: Piece) {
    _pieces = _pieces + (piece.id -> piece)
    activeUser = users.filter(_ != activeUser).head
  }

  var activeUser = users(0)
  private var _pieces = {
    val seq = for {
      u <- 0 to 1
      p <- 0 to 11
    } yield (s"p${u + 1}-${p + 1}", u, (p / 3 * 2 + p % 3 % 2 + u) % 8 + 1, p % 3 + u % 2 * 5 + 1)
    seq.map {
      case (id, u, x, y) => id -> Piece(users(u), id, Point(x, y))
    }.toMap
  }

  def pieces = _pieces

  def movablePieces = pieces.values.filter(_.user == activeUser).map(_.id)

  case class Piece(user: User, id: String, position: Point) {
    def move(pos: Point) = {
      if (math.abs(pos.x - position.x) != 1
        || pos.y - position.y != forwardDir
        || _pieces.values.exists(_.position == pos)) {
        None
      } else {
        Some(Piece(user, id, pos))
      }
    }

    def forwardDir = if (users(0) == user) 1 else -1
  }

}
