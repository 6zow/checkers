package controllers

import play.api.mvc.WebSocket
import play.api.libs.iteratee.{Enumerator, Iteratee}
import concurrent.Future
import concurrent.ExecutionContext.Implicits.global
import play.api.libs.json._
import play.api.data.validation.ValidationError
import java.util.concurrent.{LinkedBlockingQueue, BlockingQueue}

/**
 * @author Max Gorbunov
 */

case class Point(x: Double, y: Double) {
  def constraint = {
    val p = Point(math.round(x), math.round(y))
    if (p.x < 1 || p.x > 8 || p.y < 1 || p.y > 8 || (p.x + p.y) % 2 == 1) {
      None
    } else {
      Some(p)
    }
  }
}

object Point {

  /**
   * Format for Point types.
   */
  implicit object PointFormat extends Format[Point] {
    def reads(json: JsValue) = {
      def error = JsError(Seq(JsPath() -> Seq(ValidationError(s"Point ($json) is not in form {" + "\"x\": num, \"y\": num}"))))
      json match {
        case JsObject(fields) =>
          val map = fields.toMap
          if (map.size == 2) {
            JsSuccess(Point(map("x").as[Double], map("y").as[Double]))
          } else {
            error
          }
        case _ =>
          error
      }
    }

    def writes(p: Point) = Json.obj("x" -> p.x, "y" -> p.y)
  }

}

case class User(id: Int)

object Sockets {

  import Point.PointFormat

  @volatile
  var users = List[User]()
  var queues = Map[User, BlockingQueue[JsValue]]()

  var board = {
    val seq = for {
      u <- 0 to 1
      p <- 0 to 11
    } yield (s"p${u + 1}-${p + 1}", (p / 3 * 2 + p % 3 % 2 + u) % 8 + 1, p % 3 + u % 2 * 5 + 1)
    seq.map {
      case (id, x, y) => id -> Point(x, y)
    }.toMap
  }

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
              board.foreach {
                case (id, pos) => send(Json.obj("action" -> "moved", "id" -> id, "pos" -> pos))
              }
            case Some("moved") =>
              val p = (s \ "pos").as[Point]
              val id = (s \ "id").as[String]
              val pnew = p.constraint.getOrElse(board.getOrElse(id, p))
              board = board + (id -> pnew)
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
