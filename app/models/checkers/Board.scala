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
    val captured = pieceBetween(oldPiece.position, piece.position).filter(_.user != piece.user)
    val _pieces = pieces + (piece.id -> piece)
    def reportCaptured(pieces: Map[String, Piece], transform: Piece => Piece = {p => p}) {
      pieces.values.filter(_.status != Alive).foreach(p => removeCallback(transform(p)))
    }
    if (captured.isEmpty) {
      // finished
      reportCaptured(_pieces, _.remove)
      Board(users, users.filter(_ != activeUser).head, _pieces.filter(_._2.status == Alive), None)
    } else {
      // captured
      val p = captured.head
      val pieces = _pieces + (p.id -> p.die)
      val newBoard = Board(users, activeUser, pieces, Some(piece))
      if (newBoard.capturingMoves(piece).nonEmpty) {
        reportCaptured(pieces)
        newBoard
      } else {
        reportCaptured(pieces, _.remove)
        Board(users, users.filter(_ != activeUser).head, pieces.filter(_._2.status == Alive), None)
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

  def availableMoves(piece: Piece) = {
    val possibleMoves = piece.movePositions.flatMap(piece.move(_))
    if (possibleMoves.size <= 1) {
      possibleMoves.map(_.position)
    } else {
      val movedAndRemoved = possibleMoves.map {
        movedPiece => (movedPiece, pieceBetween(piece.position, movedPiece.position).headOption)
      }
      val removed = movedAndRemoved.map(_._2).toSet
      // TODO: assert that removed does not contain Some and None simultaneously
      val goodMoves = removed.flatMap {
        r =>
          val moves = movedAndRemoved.filter(_._2 == r).map(_._1)
          if (moves.size <= 1 || r.isEmpty) {
            moves
          } else {
            val captMoves = moves.filter(moved => this.updated(moved, _ => Unit).activeUser == piece.user)
            if (captMoves.size > 0) {
              captMoves
            } else {
              moves
            }
          }
      }
      goodMoves.map(_.position)
    }
  }

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


