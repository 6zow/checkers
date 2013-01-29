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
      val pieces = _pieces - p.id
      val newBoard = Board(users, activeUser, pieces, Some(piece))
      if (newBoard.capturingMoves(piece).nonEmpty) {
        newBoard
      } else {
        Board(users, users.filter(_ != activeUser).head, pieces, None)
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

  def availableMoves(piece: Piece) = piece.movePositions.flatMap(piece.move(_)).map(_.position)

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


