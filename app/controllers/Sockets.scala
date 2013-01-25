package controllers

import play.api.mvc.WebSocket
import play.api.libs.iteratee.{Enumerator, Iteratee}
import concurrent.Future
import concurrent.ExecutionContext.Implicits.global

/**
 * @author Max Gorbunov
 */
object Sockets {
  @volatile
  var events = List[String]()
  var sync = new Object

  def index = WebSocket.using[String] {
    request =>
      @volatile
      var connected = true

      // Log events to the console
      val in = Iteratee.foreach[String] {
        s =>
          println(s"Received: $s")
          events = s :: events
          sync.synchronized {
            sync.notifyAll()
          }
      }.mapDone {
        _ =>
          println("Disconnected")
          connected = false
          sync.synchronized {
            sync.notifyAll()
          }
      }

      // Send a single 'Hello!' message
      val out = Enumerator.generateM(Future {
        sync.synchronized {
          sync.wait()
        }
        if (connected) {
          Some(events.head)
        } else {
          None
        }
      })

      (in, out)
  }
}
