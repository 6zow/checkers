package models.checkers

import models.{Point, User}
import play.api.libs.json.{JsObject, Json, JsValue}
import java.util.concurrent.{LinkedBlockingQueue, BlockingQueue}
import concurrent.Future
import concurrent.ExecutionContext.Implicits.global

/**
 * @author Max Gorbunov
 */
class Game(val gameId: Int, val allUsersEqual: Boolean = false) {
  @volatile
  var users = List[User]()
  var queues = Map[User, BlockingQueue[JsValue]]()

  var boards = List(new Board(List(User(1), User(2))))

  def board = boards.head

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

  def processMessage(s: JsValue)(implicit user: User) {
    println(s"Received: $s")
    def movable(user: User) = {
      if (allUsersEqual) {
        board.movablePieces.filter(_.user == board.activeUser).map(p => p.id -> board.availableMoves(p).map(boardRef)).toMap
      } else {
        board.movablePieces.filter(_.user == user).map(p => p.id -> board.availableMoves(p).map(boardRef)).toMap
      }
    }
    (s \ "action").asOpt[String] match {
      case Some("join") =>
        send(Json.obj("msg" -> s"Welcome $user to game $gameId"))
        sendOther(Json.obj("msg" -> s"$user joined"))
        for (piece <- board.pieces.values) {
          send(Json.obj("action" -> "moved", "id" -> piece.id, "pos" -> piece.position))
        }
        send(Json.obj("movable" -> Json.toJson(movable(user))))
      case Some("moved") =>
        val p = (s \ "pos").as[Point]
        val id = (s \ "id").as[String]
        val piece = board.pieces(id)
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
        broadcast(Json.obj("action" -> "moved", "id" -> id, "pos" -> pnew))
      case Some("undo") =>
        boards = if (boards.tail.isEmpty) boards else boards.tail
        for (piece <- board.pieces.values) {
          broadcast(Json.obj("action" -> "moved", "id" -> piece.id, "pos" -> piece.position))
        }
        sendAll(user => Json.obj("movable" -> Json.toJson(movable(user))))
      case _ =>
        broadcast(s)
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
