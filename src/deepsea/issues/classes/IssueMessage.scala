package deepsea.issues.classes

import deepsea.files.classes.FileAttachment
import play.api.libs.json._

import scala.collection.mutable.ListBuffer

object IssueMessage{
  implicit val readsIssueMessage: Reads[IssueMessage] = new Reads[IssueMessage] {
    override def reads(json: JsValue): JsResult[IssueMessage] = json.asOpt[JsObject] match {
      case Some(x) => JsSuccess(new IssueMessage(
        owner = (x \ "owner").asOpt[String].getOrElse(""),
        author = (x \ "author").asOpt[String].getOrElse(""),
        content = (x \ "content").asOpt[String].getOrElse(""),
        date = (x \ "date").asOpt[Long].getOrElse(0),
        fileAttachments = (x \ "fileAttachments").asOpt[ListBuffer[FileAttachment]].getOrElse(ListBuffer.empty[FileAttachment]),
      ))
      case _ => JsSuccess(null)
    }
  }
  implicit val writesIssueMessage: Writes[IssueMessage] = new Writes[IssueMessage] {
    override def writes(o: IssueMessage): JsValue = o match {
      case x: IssueMessage => Json.obj(
        "owner" -> x.owner,
        "author" -> x.author,
        "content" -> x.content,
        "date" -> x.date,
        "fileAttachments" -> x.fileAttachments,
      )
      case _ => JsNull
    }
  }
}
class IssueMessage(val owner: String, val author: String, val content: String, val date: Long, val fileAttachments: ListBuffer[FileAttachment] = ListBuffer.empty[FileAttachment]) {

}
