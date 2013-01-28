package controllers

import play.api.mvc.WebSocket
import play.api.libs.iteratee.{Enumerator, Iteratee}
import play.api.libs.json._
import models.User
import models.checkers.Game

object Sockets {
  val games = Map(1 -> new Game(1), 0 -> new Game(0, allUsersEqual = true))

  def index(gameId: Int) = WebSocket.using[JsValue] {
    request =>
      val game = games(gameId)
      implicit val user: User = game.connectUser
      // Log events to the console
      val in = Iteratee.foreach[JsValue](game.processMessage(_)).mapDone {
        _ =>
          game.disconnectUser
      }

      val out = Enumerator.generateM(game.getMessageForUser)

      (in, out)
  }
}
