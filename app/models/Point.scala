package models

import play.api.libs.json._
import play.api.libs.json.JsSuccess
import play.api.libs.json.JsObject
import scala.Some
import play.api.data.validation.ValidationError

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