package controllers

import play.api.mvc.WebSocket
import play.api.libs.iteratee.{Enumerator, Iteratee}
import concurrent.{ExecutionContext, Future}
import ExecutionContext.Implicits.global

/**
 * @author Max Gorbunov
 */
object Sockets {
  @volatile
  var events = List[String]()

  def index = WebSocket.using[String] {
    request =>
      var count = events.length

      // Log events to the console
      val in = Iteratee.foreach[String] {
        s =>
          println(s"Received: $s")
          events = s :: events
      }.mapDone {
        _ =>
          println("Disconnected")
      }

      // Send a single 'Hello!' message
      val out = Enumerator.generateM(Future {
        while (count == events.length) {
          Thread.sleep(1000)
        }
        count = events.length
        Some(events.head)
      })

      (in, out)
  }
}
