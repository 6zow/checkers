package models.checkers

import models.{Point, User}

/**
 * @author Max Gorbunov
 */
case class Piece(user: User, id: String, position: Point, crowned: Boolean = false) {
  def move(pos: Point)(implicit board: Board) = simpleMove(pos).orElse(capturingMove(pos))

  def simpleMove(pos: Point)(implicit board: Board) = {
    if (!pos.constraint.exists(_ == pos)) {
      // invalid position
      None
    } else if (board.pieces.values.exists(_.position == pos)) {
      // there is a piece in that position
      None
    } else if (board.capturingMoves(this).nonEmpty) {
      // there are capturing moves, simple moves are not allowed
      None
    } else if (math.abs(pos.x - position.x) == 1
      && pos.y - position.y == forwardDir) {
      Some(Piece(user, id, pos, crowned || pos.y == crownRow))
    } else if (crowned && math.abs(pos.x - position.x) == math.abs(pos.y - position.y)) {
      val v = Point(-math.signum(pos.x - position.x), -math.signum(pos.y - position.y))
      lazy val s: Stream[Point] = pos #:: s.map(_ + v)
      if (s.takeWhile(_ != position).exists(board.pieceAt(_).nonEmpty)) {
        None
      } else {
        Some(Piece(user, id, pos, crowned))
      }
    } else {
      None
    }
  }

  def capturingMove(pos: Point)(implicit board: Board) = {
    if (!pos.constraint.exists(_ == pos)) {
      // invalid position
      None
    } else if (board.pieces.values.exists(_.position == pos)) {
      // there is a piece in that position
      None
    } else if (math.abs(pos.y - position.y) == math.abs(pos.x - position.x)
      && math.abs(pos.x - position.x) <= (if (crowned) 100 else 2)
      && board.pieceBetween(pos, position).filter(_.user != user).size == 1) {
      // capture
      Some(Piece(user, id, pos, crowned || pos.y == crownRow))
    } else {
      None
    }
  }

  def movePositions(implicit board: Board): Iterable[Point] = {
    def streamInDir(dir: Point) = {
      lazy val stream: Stream[Point] = (position + dir) #:: stream.map(_ + dir)
      stream.takeWhile(_.constraint.nonEmpty)
    }
    if (crowned) {
      streamInDir(Point(-1, -1)) ++ streamInDir(Point(1, -1)) ++ streamInDir(Point(-1, 1)) ++ streamInDir(Point(1, 1))
    } else {
      List(Point(-1, forwardDir), Point(1, forwardDir), Point(-2, -2), Point(2, -2), Point(-2, 2), Point(2, 2)).
        map(_ + position).filter(_.constraint.nonEmpty)
    }
  }

  def forwardDir(implicit board: Board) = if (board.users(0) == user) 1 else -1

  def crownRow(implicit board: Board) = if (board.users(0) == user) 8 else 1
}
