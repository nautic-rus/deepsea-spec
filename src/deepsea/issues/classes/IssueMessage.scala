package deepsea.issues.classes

import play.api.libs.json._

object IssueMessage{
  implicit val readsIssueMessage: Reads[IssueMessage] = new Reads[IssueMessage] {
    override def reads(json: JsValue): JsResult[IssueMessage] = json.asOpt[JsObject] match {
      case Some(x) => JsSuccess(new IssueMessage(
        author = (x \ "author").asOpt[String].getOrElse(""),
        content = (x \ "content").asOpt[String].getOrElse(""),
        date = (x \ "date").asOpt[Long].getOrElse(0),
      ))
      case _ => JsSuccess(null)
    }
  }
  implicit val writesIssueMessage: Writes[IssueMessage] = new Writes[IssueMessage] {
    override def writes(o: IssueMessage): JsValue = o match {
      case x: IssueMessage => Json.obj(
        "author" -> x.author,
        "content" -> x.content,
        "date" -> x.date,
      )
      case _ => JsNull
    }
  }
}
class IssueMessage(val author: String, val content: String, val date: Long) {

}
