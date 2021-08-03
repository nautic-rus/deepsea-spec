package deepsea.issues.classes

import play.api.libs.json._

object Issue{
  implicit val writesIssue: Writes[Issue] = new Writes[Issue] {
    override def writes(o: Issue): JsValue = o match {
      case x: Issue => Json.obj(
        "id" -> x.id,
        "status" -> x.status,
        "project" -> x.project,
        "department" -> x.department,
        "startedBy" -> x.startedBy,
        "taskType" -> x.taskType,
        "name" -> x.name,
        "details" -> x.details,
        "assignedTo" -> x.assignedTo,
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
        taskType = (x \ "taskType").asOpt[String].getOrElse(""),
        name = (x \ "name").asOpt[String].getOrElse(""),
        details = (x \ "details").asOpt[String].getOrElse(""),
        assignedTo = (x \ "assignedTo").asOpt[String].getOrElse(""),
      ))
      case _ => JsSuccess (null)
    }
  }
}
class Issue(var id: String, var status: String, var project: String, var department: String, var startedBy: String, var taskType: String, var name: String, var details: String, var assignedTo: String) {


}
