package models.checkers

import models.Point
import org.specs2.mutable._
import models.User

/**
 * @author Max Gorbunov
 */
class CheckersSpec extends Specification {

  "Crowned piece" should {

    "not continue moving when nothing to capture" in {
      val u1 = User(0)
      val u2 = User(1)
      val p3 = Piece(u2, "3", Point(7, 3))
      implicit val board = Board(List(u1, u2), u2,
        List(
          Piece(u1, "1", Point(3, 1)),
          Piece(u1, "2", Point(6, 2)),
          p3
        ).map(p => p.id -> p).toMap, None)
      val moves = board.availableMoves(p3)
      moves.size mustEqual 1
      val board1 = board.updated(p3.move(moves.head).get, p => Unit)
      board1.activeUser mustEqual u1
    }

    "not ignore pieces of same color when capturing" in {
      val u1 = User(0)
      val u2 = User(1)
      val p3 = Piece(u2, "3", Point(5, 1), crowned = true)
      implicit val board = Board(List(u1, u2), u2,
        List(
          Piece(u1, "1", Point(4, 2)),
          Piece(u2, "2", Point(3, 3)),
          p3
        ).map(p => p.id -> p).toMap, None)
      val moves = board.availableMoves(p3)
      moves.size mustEqual 3
      p3.move(Point(1, 5)) mustEqual None
      p3.move(Point(2, 4)) mustEqual None
      p3.move(Point(3, 3)) mustEqual None
    }
  }
}
