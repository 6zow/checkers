package models.checkers

import models.Point
import org.specs2.mutable._
import models.User

/**
 * @author Max Gorbunov
 */
class CheckersSpec extends Specification {

  val u1 = User(0)
  val u2 = User(1)

  "Crowned piece" should {

    "not continue moving when nothing to capture" in {
      val p3 = Piece(u2, "3", Point(7, 3))
      implicit val board = Board(List(u1, u2), u2,
        List(
          Piece(u1, "1", Point(3, 1)),
          Piece(u1, "2", Point(6, 2)),
          p3
        ).map(p => p.id -> p).toMap, None)
      val moves = board.availableMoves(p3)
      "- There must be only one available move" in {
        moves must have size 1
      }
      val board1 = board.updated(p3.move(moves.head).get, p => Unit)
      board1.activeUser mustEqual u1
    }

    "not ignore pieces of same color when capturing" in {
      val p3 = Piece(u2, "3", Point(5, 1), crowned = true)
      implicit val board = Board(List(u1, u2), u2,
        List(
          Piece(u1, "1", Point(4, 2)),
          Piece(u2, "2", Point(3, 3)),
          p3
        ).map(p => p.id -> p).toMap, None)
      val moves = board.availableMoves(p3)
      moves must have size 3
      p3.move(Point(1, 5)) mustEqual None
      p3.move(Point(2, 4)) mustEqual None
      p3.move(Point(3, 3)) mustEqual None
    }

    "capture next piece even if there is a way to avoid it" in {
      val p3 = Piece(u2, "3", Point(8, 4), crowned = true)
      implicit val board = Board(List(u1, u2), u2,
        List(
          Piece(u1, "1", Point(7, 3)),
          Piece(u1, "2", Point(5, 3)),
          p3
        ).map(p => p.id -> p).toMap, None)
      val moves = board.availableMoves(p3)
      moves mustEqual Set(Point(6, 2))
    }

    "be unable to capture 2 pieces if it's between them" in {
      val p3 = Piece(u2, "3", Point(4, 4), crowned = true)
      implicit val board = Board(List(u1, u2), u2,
        List(
          Piece(u1, "1", Point(5, 5)),
          Piece(u1, "2", Point(3, 3)),
          p3
        ).map(p => p.id -> p).toMap, None)
      val board1 = board.updated(p3.move(Point(6, 6)).get, _ => Unit)
      "- Active user must switch" in {
        board1.activeUser mustEqual u1
      }
    }
  }

  "Dead piece" should {

    "block moves" in {
      val p3 = Piece(u1, "3", Point(3, 1))
      implicit val board = Board(List(u1, u2), u1,
        List(
          Piece(u2, "1", Point(2, 2), status = Dead),
          Piece(u2, "2", Point(4, 2), status = Dead),
          p3
        ).map(p => p.id -> p).toMap, None)
      val moves = board.availableMoves(p3)
      "- There must be no available moves" in {
        moves must beEmpty
      }
    }

    "block crowned moves" in {
      val p3 = Piece(u1, "3", Point(3, 1), crowned = true)
      implicit val board = Board(List(u1, u2), u1,
        List(
          Piece(u2, "1", Point(2, 2), status = Dead),
          Piece(u2, "2", Point(4, 2), status = Dead),
          p3
        ).map(p => p.id -> p).toMap, None)
      val moves = board.availableMoves(p3)
      "- There must be no available moves" in {
        moves must beEmpty
      }
    }

    "block crowned captures" in {
      val p3 = Piece(u1, "3", Point(3, 1), crowned = true)
      implicit val board = Board(List(u1, u2), u1,
        List(
          Piece(u2, "1", Point(2, 2), status = Dead),
          Piece(u2, "2", Point(4, 2)),
          p3,
          Piece(u2, "4", Point(5, 3), status = Dead)
        ).map(p => p.id -> p).toMap, None)
      val moves = board.availableMoves(p3)
      "- There must be no available moves" in {
        moves.toSet must beEmpty
      }
    }

    "be removed when switching activeUser" in {
      val p3 = Piece(u1, "3", Point(3, 1))
      implicit val board = Board(List(u1, u2), u1,
        List(
          Piece(u2, "1", Point(5, 5), status = Dead),
          Piece(u2, "2", Point(4, 2)),
          p3
        ).map(p => p.id -> p).toMap, None)
      val moves = board.availableMoves(p3)
      moves.toSet mustEqual Set(Point(5, 3))
      val board1 = board.updated(p3.move(moves.head).get, _ => Unit)
      board1.pieces.values must not have {
        p: Piece => p.status != Alive
      }
    }

    "not be removed when not switching activeUser" in {
      val p3 = Piece(u1, "3", Point(3, 1))
      implicit val board = Board(List(u1, u2), u1,
        List(
          Piece(u2, "1", Point(5, 5), status = Dead),
          Piece(u2, "2", Point(4, 2)),
          p3,
          Piece(u2, "4", Point(6, 4))
        ).map(p => p.id -> p).toMap, None)
      val moves = board.availableMoves(p3)
      moves.toSet mustEqual Set(Point(5, 3))
      val board1 = board.updated(p3.move(moves.head).get, _ => Unit)
      board1.pieces.values must have {
        p: Piece => p.status == Dead
      }
    }
  }

  "Piece" should {

    "be marked dead and not removed when captured and user is not switched" in {
      val p3 = Piece(u2, "3", Point(8, 4))
      implicit val board = Board(List(u1, u2), u2,
        List(
          Piece(u1, "1", Point(7, 3)),
          Piece(u1, "2", Point(5, 3)),
          p3
        ).map(p => p.id -> p).toMap, None)
      val moves = board.availableMoves(p3)
      moves.toSet mustEqual Set(Point(6, 2))
      var removed = Option.empty[Piece]
      val board1 = board.updated(p3.move(moves.head).get, {
        p => removed = Some(p)
      })
      "- There must be a captured piece" in {
        removed must not be empty
      }
      "- The captured piece must not be removed" in {
        board1.pieceAt(removed.get.position) must not be empty
      }
      "- The captured piece must be marked as dead" in {
        board1.pieceAt(removed.get.position).get mustEqual Piece(u1, "1", Point(7, 3), status = Dead)
      }
    }
  }
}
