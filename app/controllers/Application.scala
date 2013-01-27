package controllers

import play.api.mvc._

object Application extends Controller {
  
  def index(id: Int) = Action {
    Ok(views.html.index(id))
  }

  def index0 = index(0)
}
