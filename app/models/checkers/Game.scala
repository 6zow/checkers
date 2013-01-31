package models.checkers

import models.{Point, User}
import play.api.libs.json.{JsObject, Json, JsValue}
import java.util.concurrent.{LinkedBlockingQueue, BlockingQueue}
import concurrent.Future
import concurrent.ExecutionContext.Implicits.global

/**
 * @author Max Gorbunov
 */
class Game(val gameId: Int, val allUsersEqual: Boolean = false, computer: Int = 0) {
  @volatile
  var users = List[User]()
  var queues = Map[User, BlockingQueue[JsValue]]()

  var boards = computer match {
    case 1 => List(new Board(List(User(-1), User(1))))
    case 2 => List(new Board(List(User(1), User(-1))))
    case 3 => List(new Board(List(User(-1), User(-2))))
    case _ => List(new Board(List(User(1), User(2))))
  }

  implicit def board = boards.head

  def broadcast(event: JsValue) {
    for (user <- users) {
      send(event)(user)
    }
  }

  def sendAll(event: User => JsValue) {
    for (user <- users) {
      send(event(user))(user)
    }
  }

  def sendOther(event: JsValue)(implicit self: User) {
    println(s"sendOther: $event")
    for (user <- users) {
      if (user != self) {
        send(event)(user)
      }
    }
  }

  def send(event: JsValue)(implicit user: User) {
    println(s"to $user: $event")
    queues(user).offer(event)
  }

  def boardRef(p: Point) = (p.x + Char.char2int('A') - 1).toChar + "" + p.y.toInt


  // Communication part

  def connectUser: User = {
    val user = User((1 to 1000).dropWhile(id => users.exists(_.id == id)).head)

    users = user :: users
    queues = queues + (user -> new LinkedBlockingQueue[JsValue]())
    user
  }

  var freeMove = false

  def processMessage(s: JsValue)(implicit user: User) {
    println(s"Received: $s")
    def movable(user: User) = {
      if (freeMove) {
        val x = (1 to 8).flatMap(i => (1 to 8).map(j => Point(i, j))).filter(board.pieceAt(_).isEmpty).map(boardRef)
        board.pieces.values.map(_.id -> x).toMap
      } else if (allUsersEqual) {
        board.movablePieces.filter(_.user == board.activeUser).map(p => p.id -> board.availableMoves(p).map(boardRef)).toMap
      } else {
        board.movablePieces.filter(_.user == user).map(p => p.id -> board.availableMoves(p).map(boardRef)).toMap
      }
    }
    def movedJson(piece: Piece) =
      Json.obj("action" -> "moved", "id" -> piece.id, "pos" -> piece.position, "crowned" -> piece.crowned)
    def capturedJson(piece: Piece) =
      Json.obj("action" -> (if (piece.status == Dead) "died" else "removed"), "id" -> piece.id)
    (s \ "action").asOpt[String] match {
      case Some("join") =>
        send(Json.obj("msg" -> s"Welcome $user to game $gameId"))
        sendOther(Json.obj("msg" -> s"$user joined"))
        for (piece <- board.pieces.values) {
          send(movedJson(piece))
        }
        board.pieces.values.filter(_.status != Alive).foreach(p => send(capturedJson(p)))
        send(Json.obj("movable" -> Json.toJson(movable(user))))
      case Some("moved") =>
        val p = (s \ "pos").as[Point]
        val id = (s \ "id").as[String]
        val piece = board.pieces(id)
        if (freeMove) {
          val newPiece = Piece(piece.user, id, p.constraint.filter(board.pieceAt(_).isEmpty).getOrElse(Point(0, if (users(0) == piece.user) 1 else 2)), piece.crowned)
          boards = Board(board.users, board.activeUser, board.pieces - id + (id -> newPiece), board.capturingPiece) :: boards
          broadcast(movedJson(newPiece))
          sendAll(user => Json.obj("movable" -> Json.toJson(movable(user))))
        } else {
          val newPiece = p.constraint.flatMap(piece.move(_)(board))
          val pnew = newPiece match {
            case Some(newp) =>
              boards = board.updated(newp, piece => broadcast(capturedJson(piece))) :: boards
              sendAll(user => Json.obj("movable" -> Json.toJson(movable(user))))
              newp.position
            case None =>
              piece.position
          }
          println(s"$p -> $pnew")
          broadcast(movedJson(board.pieces(id)))
          while (board.activeUser.id < 0) {
            broadcast(Json.obj("msg" -> "Computer is thinking"))
            Thread.sleep(1000)
            val moves = board.movablePieces.toStream.flatMap(p => board.availableMoves(p).map((p, _)))
            val piece = if (moves.nonEmpty && moves.tail.isEmpty) {
              val (p, pos) = moves.head
              p.move(pos).get
            } else {
              computeBestMove(board, board.activeUser)._1
            }
            boards = board.updated(piece, removed => broadcast(capturedJson(removed))) :: boards
            sendAll(user => Json.obj("movable" -> Json.toJson(movable(user))))
            broadcast(movedJson(board.pieces(piece.id)))
          }
        }
      case Some("undo") =>
        boards = if (boards.tail.isEmpty) boards else boards.tail
        for (piece <- board.pieces.values) {
          broadcast(movedJson(piece))
        }
        sendAll(user => Json.obj("movable" -> Json.toJson(movable(user))))
      case _ =>
        // TODO: don't broadcast everything without verification
        broadcast(s)
    }
    (s \ "freeMove").asOpt[Boolean] match {
      case Some(true) =>
        freeMove = true
        sendAll(user => Json.obj("movable" -> Json.toJson(movable(user))))
      case Some(false) =>
        freeMove = false
        sendAll(user => Json.obj("movable" -> Json.toJson(movable(user))))
      case _ =>
    }
  }

  def estimate(board: Board, user: User): (Piece, Double) = {
    val pieces = board.pieces.values
    (pieces.head, pieces.count(_.user == user) - pieces.count(_.user != user))
  }

  def computeBestMove(implicit board: Board, user: User, depth: Int = 7): (Piece, Double) = {
    if (depth == 0) {
      estimate(board, user)
    } else {
      val options = for {
        piece <- board.movablePieces
        move <- board.availableMoves(piece)
        newPiece <- piece.move(move)
        board1 = board.updated(newPiece, p => Unit)
        (_, estimate) = computeBestMove(board1, user, depth - 1)
      } yield (newPiece, estimate)
      if (options.isEmpty) {
        estimate(board, user)
      } else {
        val getMax = board.activeUser == user
        val best = options.foldLeft((Piece(user, "", Point(0, 0)), if (getMax) -1e100 else 1e100)) {
          case ((p1, e1), (p2, e2)) =>
            if (getMax && e1 > e2 || !getMax && e1 < e2) (p1, e1) else (p2, e2)
        }
        best
      }
    }
  }

  def disconnectUser(implicit user: User) {
    println("Disconnected")
    send(Json.obj())
  }

  def getMessageForUser(implicit user: User) = Future[Option[JsValue]] {
    println(s"awaiting for $user")
    queues(user).take match {
      case JsObject(Nil) => {
        users = users.filterNot(_ == user)
        queues = queues - user
        None
      }
      case msg => Some(msg)
    }
  }
}
