import spray.json._

class RichJsValue(js: JsValue) {
  def \ (name: String): JsValue = js match {
    case JsObject(fields) =>
      fields(name)
    case _ =>
      throw new IllegalArgumentException("Cannot select field "+ name +" from non-JsObject "+ js)
  }

  def hasFieldNamed(name: String) = js match {
    case JsObject(fields) =>
      fields.contains(name)
    case _ =>
      false
  }

  def arrayValues: Vector[JsValue] = js match {
    case JsArray(values) =>
      values
    case _ =>
      throw new IllegalArgumentException("Trying to select values from non-JsArray"+ js)
  }
}

object RichJsValue {
  implicit def enrichJsValue(js: JsValue) = new RichJsValue(js)
}
