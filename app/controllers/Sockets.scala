package controllers

import play.api.mvc.WebSocket
import play.api.libs.iteratee.{Enumerator, Iteratee}
import concurrent.Future
import concurrent.ExecutionContext.Implicits.global
import play.api.libs.json._
import java.util.concurrent.{LinkedBlockingQueue, BlockingQueue}
import models.{User, Point}
import models.checkers.Board

object Sockets {

  import Point.PointFormat

  @volatile
  var users = List[User]()
  var queues = Map[User, BlockingQueue[JsValue]]()

  val board = new Board()

  def broadcast(event: JsValue) {
    println(s"broadcast: $event")
    for (user <- users) {
      send(event)(user)
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

  def index = WebSocket.using[JsValue] {
    request =>
      @volatile
      implicit var user = User((1 to 1000).dropWhile(id => users.exists(_.id == id)).head)

      users = user :: users
      queues = queues + (user -> new LinkedBlockingQueue[JsValue]())
      // Log events to the console
      val in = Iteratee.foreach[JsValue] {
        s =>
          println(s"Received: $s")
          (s \ "action").asOpt[String] match {
            case Some("join") =>
              send(Json.obj("msg" -> "welcome"))
              sendOther(Json.obj("msg" -> s"$user joined"))
              board.pieces.foreach {
                case (id, pos) => send(Json.obj("action" -> "moved", "id" -> id, "pos" -> pos))
              }
            case Some("moved") =>
              val p = (s \ "pos").as[Point]
              val id = (s \ "id").as[String]
              val pnew = p.constraint.getOrElse(board.pieces.getOrElse(id, p))
              board.pieces = board.pieces + (id -> pnew)
              println(s"$p -> $pnew")
              broadcast(Json.obj("action" -> "moved", "id" -> id, "pos" -> pnew))
            case _ =>
              broadcast(s)
          }
      }.mapDone {
        _ =>
          println("Disconnected")
          send(Json.obj())
      }

      val out = Enumerator.generateM(Future[Option[JsValue]] {
        println(s"awaiting for $user")
        queues(user).take match {
          case JsObject(Nil) => {
            users = users.filterNot(_ == user)
            queues = queues - user
            None
          }
          case msg => Some(msg)
        }
      })

      (in, out)
  }
}
