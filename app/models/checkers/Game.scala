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
    (s \ "action").asOpt[String] match {
      case Some("join") =>
        send(Json.obj("msg" -> s"Welcome $user to game $gameId"))
        sendOther(Json.obj("msg" -> s"$user joined"))
        for (piece <- board.pieces.values) {
          send(movedJson(piece))
        }
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
              boards = board.updated(newp, piece => broadcast(Json.obj("action" -> "removed", "id" -> piece.id))) :: boards
              sendAll(user => Json.obj("movable" -> Json.toJson(movable(user))))
              newp.position
            case None =>
              piece.position
          }
          println(s"$p -> $pnew")
          broadcast(movedJson(board.pieces(id)))
          while (board.activeUser.id < 0) {
            Thread.sleep(1000)
            broadcast(Json.obj("msg" -> "Computer is thinking"))
            val compPiece = board.movablePieces.head
            val move = board.availableMoves(compPiece).head
            boards = board.updated(compPiece.move(move).get, piece => broadcast(Json.obj("action" -> "removed", "id" -> piece.id))) :: boards
            sendAll(user => Json.obj("movable" -> Json.toJson(movable(user))))
            broadcast(movedJson(board.pieces(compPiece.id)))
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
