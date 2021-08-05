package deepsea.files.classes

import play.api.libs.json._

object FileAttachment{
  implicit val reads: Reads[FileAttachment] = new Reads[FileAttachment] {
    override def reads(json: JsValue): JsResult[FileAttachment] = json.asOpt[JsObject] match {
      case Some(x) => JsSuccess(new FileAttachment(
        name = (x \ "name").asOpt[String].getOrElse(""),
        url = (x \ "url").asOpt[String].getOrElse(""),
      ))
      case _ => JsSuccess(null)
    }
  }
  implicit val writes: Writes[FileAttachment] = new Writes[FileAttachment] {
    override def writes(o: FileAttachment): JsValue = o match {
      case x: FileAttachment => Json.obj(
        "name" -> x.name,
        "url" -> x.url,
      )
      case _ => JsNull
    }
  }
}
class FileAttachment(val name: String, val url: String) {

}
