package deepsea.issues.classes

import deepsea.files.classes.FileAttachment
import play.api.libs.json._

import scala.collection.mutable.ListBuffer

object Issue{
  implicit val writesIssue: Writes[Issue] = new Writes[Issue] {
    override def writes(o: Issue): JsValue = o match {
      case x: Issue => Json.obj(
        "id" -> x.id,
        "status" -> x.status,
        "project" -> x.project,
        "department" -> x.department,
        "startedBy" -> x.startedBy,
        "startedDate" -> x.startedDate,
        "taskType" -> x.taskType,
        "name" -> x.name,
        "assignedTo" -> x.assignedTo,
        "details" -> x.details,
        "messages" -> x.messages,
        "availableStatuses" -> x.availableStatuses,
        "fileAttachments" -> x.fileAttachments
      )
      case _ => JsNull
    }
  }
  implicit val readsIssue: Reads[Issue] = new Reads[Issue] {
    override def reads (json: JsValue): JsResult[Issue] = json.asOpt[JsObject] match {
      case Some (x) => JsSuccess (new Issue(
        id = (x \ "id").asOpt[String].getOrElse(""),
        status = (x \ "status").asOpt[String].getOrElse(""),
        project = (x \ "project").asOpt[String].getOrElse(""),
        department = (x \ "department").asOpt[String].getOrElse(""),
        startedBy = (x \ "startedBy").asOpt[String].getOrElse(""),
        startedDate = (x \ "startedDate").asOpt[Long].getOrElse(0),
        taskType = (x \ "taskType").asOpt[String].getOrElse(""),
        name = (x \ "name").asOpt[String].getOrElse(""),
        details = (x \ "details").asOpt[String].getOrElse(""),
        assignedTo = (x \ "assignedTo").asOpt[String].getOrElse(""),
        messages = (x \ "messages").asOpt[ListBuffer[IssueMessage]].getOrElse(ListBuffer.empty[IssueMessage]),
      ){
        availableStatuses = (x \ "availableStatuses").asOpt[ListBuffer[String]].getOrElse(ListBuffer.empty[String])
        fileAttachments = (x \ "fileAttachments").asOpt[ListBuffer[FileAttachment]].getOrElse(ListBuffer.empty[FileAttachment])
      })
      case _ => JsSuccess (null)
    }
  }
}
class Issue(var id: String, var status: String, var project: String, var department: String, var startedBy: String,
            var startedDate: Long, var taskType: String, var name: String, var details: String, var assignedTo: String,
            var messages: ListBuffer[IssueMessage] = ListBuffer.empty[IssueMessage]) {
  var availableStatuses: ListBuffer[String] = ListBuffer.empty[String]
  var fileAttachments: ListBuffer[FileAttachment] = ListBuffer.empty[FileAttachment]

}
