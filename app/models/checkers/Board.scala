package models.checkers

import models.{User, Point}

/**
 * @author Max Gorbunov
 */
class Board(val users: List[User]) {
  def update(piece: Piece, removeCallback: Piece => Unit) {
    val oldPiece = _pieces(piece.id)
    val remove = pieceBetween(oldPiece.position, piece.position).filter(_.user != piece.user)
    _pieces = _pieces + (piece.id -> piece)
    if (remove.isEmpty) {
      // finished
      changeActiveUser()
    } else {
      // captured
      val p = remove.head
      _pieces = _pieces - p.id
      removeCallback(p)
      if (capturingMoves(piece).nonEmpty) {
        capturingPiece = Some(piece)
      } else {
        changeActiveUser()
        capturingPiece = None
      }
    }
  }

  def changeActiveUser() {
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

  var capturingPiece: Option[Piece] = None

  def pieces = _pieces

  def movablePieces = capturingPiece.map(Seq(_)).getOrElse {
    if (capturingPieces.nonEmpty) {
      capturingPieces
    } else {
      pieces.values.filter(p => p.user == activeUser && availableMoves(p).nonEmpty)
    }
  }

  def capturingMoves(piece: Piece) = piece.movePositions.flatMap(piece.capturingMove).map(_.position)

  def capturingPieces = pieces.values.filter {
    p => p.user == activeUser && capturingMoves(p).nonEmpty
  }

  def availableMoves(piece: Piece) = piece.movePositions.flatMap(piece.move).map(_.position) ++ capturingMoves(piece)

  def pieceBetween(p1: Point, p2: Point) = {
    val d = math.abs(p1.x - p2.x)
    for {
      i <- 1 to (d.toInt - 1)
      piece <- pieceAt(p1 * (1 - i / d) + p2 * (i / d))
    } yield piece
  }

  def pieceAt(position: Point): Option[Piece] = {
    _pieces.values.filter(_.position == position).headOption
  }

  case class Piece(user: User, id: String, position: Point, crowned: Boolean = false) {
    def move(pos: Point) = simpleMove(pos).orElse(capturingMove(pos))

    def simpleMove(pos: Point) = {
      if (!pos.constraint.exists(_ == pos)) {
        // invalid position
        None
      } else if (_pieces.values.exists(_.position == pos)) {
        // there is a piece in that position
        None
      } else if (capturingMoves(this).nonEmpty) {
        // there are capturing moves, simple moves are not allowed
        None
      } else if (math.abs(pos.x - position.x) == 1
        && pos.y - position.y == forwardDir) {
        Some(Piece(user, id, pos, crowned || pos.y == crownRow))
      } else if (crowned && math.abs(pos.x - position.x) == math.abs(pos.y - position.y)) {
        val v = Point(-math.signum(pos.x - position.x), -math.signum(pos.y - position.y))
        lazy val s: Stream[Point] = pos #:: s.map(_ + v)
        if (s.takeWhile(_ != position).exists(pieceAt(_).nonEmpty)) {
          None
        } else {
          Some(Piece(user, id, pos, crowned))
        }
      } else {
        None
      }
    }

    def capturingMove(pos: Point) = {
      val maxDist = if (crowned) 100 else 2
      if (!pos.constraint.exists(_ == pos)) {
        // invalid position
        None
      } else if (_pieces.values.exists(_.position == pos)) {
        // there is a piece in that position
        None
      } else if (math.abs(pos.y - position.y) == math.abs(pos.x - position.x)
        && math.abs(pos.x - position.x) <= maxDist
        && pieceBetween(pos, position).filter(_.user != user).size == 1) {
        // capture
        Some(Piece(user, id, pos, crowned || pos.y == crownRow))
      } else {
        None
      }
    }

    def movePositions: Iterable[Point] = {
      def streamInDir(dir: Point) = {
        lazy val stream: Stream[Point] = position #:: stream.map(_ + dir)
        stream.takeWhile(_.constraint.nonEmpty)
      }
      if (crowned) {
        streamInDir(Point(-1, -1)) ++ streamInDir(Point(1, -1)) ++ streamInDir(Point(-1, 1)) ++ streamInDir(Point(1, 1))
      } else {
        List(Point(-1, forwardDir), Point(1, forwardDir), Point(-2, -2), Point(2, -2), Point(-2, 2), Point(2, 2)).
          map(_ + position)
      }
    }

    def forwardDir = if (users(0) == user) 1 else -1

    def crownRow = if (users(0) == user) 8 else 1
  }

}
