package models.checkers

import models.{User, Point}

/**
 * @author Max Gorbunov
 */
object Board {
  def defaultPieces(users: List[User]) = {
    val seq = for {
      u <- 0 to 1
      p <- 0 to 11
    } yield Piece(users(u), s"p${u + 1}-${p + 1}", Point((p / 3 * 2 + p % 3 % 2 + u) % 8 + 1, p % 3 + u % 2 * 5 + 1))
    seq.map(piece => piece.id -> piece).toMap
  }
}

case class Board(users: List[User], activeUser: User, pieces: Map[String, Piece], capturingPiece: Option[Piece]) {
  def this(users: List[User]) = this(users, users(0), Board.defaultPieces(users), None)

  implicit val self = this

  def updated(piece: Piece, removeCallback: Piece => Unit): Board = {
    val oldPiece = pieces(piece.id)
    val remove = pieceBetween(oldPiece.position, piece.position).filter(_.user != piece.user)
    val _pieces = pieces + (piece.id -> piece)
    if (remove.isEmpty) {
      // finished
      Board(users, users.filter(_ != activeUser).head, _pieces, None)
    } else {
      // captured
      val p = remove.head
      removeCallback(p)
      if (capturingMoves(piece).nonEmpty) {
        Board(users, activeUser, _pieces - p.id, Some(piece))
      } else {
        Board(users, users.filter(_ != activeUser).head, _pieces - p.id, None)
      }
    }
  }

  def movablePieces = capturingPiece.map(Seq(_)).getOrElse {
    val capturing = capturingPieces
    if (capturing.nonEmpty) {
      capturing
    } else {
      pieces.values.filter(p => p.user == activeUser && availableMoves(p).nonEmpty)
    }
  }

  def capturingMoves(piece: Piece) = piece.movePositions.flatMap(piece.capturingMove(_)).map(_.position)

  def capturingPieces = pieces.values.filter {
    p => p.user == activeUser && capturingMoves(p).nonEmpty
  }

  def availableMoves(piece: Piece) = piece.movePositions.flatMap(piece.move(_)).map(_.position) ++ capturingMoves(piece)

  def pieceBetween(p1: Point, p2: Point) = {
    val d = math.abs(p1.x - p2.x)
    for {
      i <- 1 to (d.toInt - 1)
      point <- (p1 * (1 - i / d) + p2 * (i / d)).constraint
      piece <- pieceAt(point)
    } yield piece
  }

  def pieceAt(position: Point): Option[Piece] = {
    pieces.values.filter(_.position == position).headOption
  }
}

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
