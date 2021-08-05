package deepsea.issues

import akka.actor.Actor
import akka.pattern.ask
import akka.util.Timeout
import deepsea.actors.ActorManager
import deepsea.auth.AuthManager.{GetUser, User}
import deepsea.camunda.CamundaManager._
import deepsea.database.DatabaseManager.GetConnection
import deepsea.files.classes.FileAttachment
import deepsea.issues.IssueManager.{GetIssueDetails, GetIssues, InitIssue, IssueDef, ProcessIssue, RemoveIssue, SetIssueMessage, SetIssueStatus}
import deepsea.issues.classes.{Issue, IssueMessage}
import play.api.libs.json.{Json, OWrites}

import java.util.concurrent.TimeUnit
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await

object IssueManager{

  case class GetIssues(user: String)
  case class InitIssue(user: String)
  case class StartIssue(user: String)
  case class ProcessIssue(issue: String)
  case class RemoveIssue(id: String)
  case class GetIssueProjects()
  case class GetIssueTypes()
  case class GetIssueMessages(id: String)
  case class GetIssueDetails(id: String, user: String)
  case class SetIssueStatus(id: String, user: String, status: String)
  case class SetIssueMessage(id: String, message: IssueMessage)

  case class IssueDef(id: String, issueTypes: List[String], issueProjects: List[String])
  implicit val writesIssueDef: OWrites[IssueDef] = Json.writes[IssueDef]


  case class IdName(id: Int, name: String)
  implicit val writesUser: OWrites[IdName] = Json.writes[IdName]


}
class IssueManager extends Actor{
  implicit val timeout: Timeout = Timeout(30, TimeUnit.SECONDS)

  override def receive: Receive = {
    case InitIssue(user) =>
      Await.result(ActorManager.camunda ? InitIssueInstance(user), timeout.duration) match {
        case result: IssueDef => sender() ! Json.toJson(result)
        case _ => sender() ! Json.toJson(IssueDef("", List.empty[String], List.empty[String]))
      }
    case ProcessIssue(issueJson) =>
      Json.parse(issueJson).asOpt[Issue] match {
        case Some(issue) =>
          Await.result(ActorManager.camunda ? ProcessIssueInstance(issue), timeout.duration) match {
            case result: String =>
              issue.fileAttachments.foreach(x =>  setIssueFileAttachments(issue.id, x))
              sender() ! Json.toJson(result)
            case _ => sender() ! Json.toJson("error")
          }
        case _ => None
      }
    case GetIssues(userName) =>
      Await.result(ActorManager.auth ? GetUser(userName), timeout.duration) match {
        case user: User =>
          Await.result(ActorManager.camunda ? GetIssuesForUser(user), timeout.duration) match {
            case result: ListBuffer[Issue] => sender() ! Json.toJson(result)
            case _ => sender() ! Json.toJson(ListBuffer.empty[Issue])
          }
        case _ => sender() ! Json.toJson(ListBuffer.empty[Issue])
      }
    case RemoveIssue(id) =>
      Await.result(ActorManager.camunda ? RemoveIssueInstance(id), timeout.duration) match {
        case result: String => sender() ! Json.toJson(result)
        case _ => sender() ! Json.toJson("error")
      }
    case GetIssueDetails(id, userName) =>
      Await.result(ActorManager.auth ? GetUser(userName), timeout.duration) match {
        case user: User =>
          Await.result(ActorManager.camunda ? GetIssueInstanceDetails(id, user, getIssueMessages(id)), timeout.duration) match {
            case result: Issue => sender() ! Json.toJson(result)
            case _ => sender() ! Json.toJson("error")
          }
        case _ => sender() ! Json.toJson("error")
      }
    case SetIssueStatus(id, user, status) =>
      Await.result(ActorManager.camunda ? SetIssueInstanceStatus(id, user, status), timeout.duration) match {
        case result: Issue => sender() ! Json.toJson(result)
        case _ => sender() ! Json.toJson("error")
      }
    case SetIssueMessage(id, message) =>
      setIssueMessage(id, message)
    case _ => None
  }

  def getIssueMessages(id: String): ListBuffer[IssueMessage] ={
    val res = ListBuffer.empty[IssueMessage]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from issue_messages where issue_id = $id and removed = 0")
        while (rs.next()){
          res += new IssueMessage(
            "human",
            rs.getString("author"),
            rs.getString("content"),
            rs.getLong("date"),
          ){
            fileAttachments = getMessageFileAttachments(id)
          }
        }
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def setIssueMessage(id: String, message: IssueMessage): Unit ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"insert into issue_messages (issue_id, author, content, date, file_attachments) values ('$id', '${message.author}', '${message.content}', '${message.date}', '${Json.toJson(message.fileAttachments).toString()}')")
        c.close()
      case _ =>
    }
  }
  def getMessageFileAttachments(id: String): ListBuffer[FileAttachment] ={
    val res = ListBuffer.empty[FileAttachment]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from file_attachments where message_id = $id")
        while (rs.next()){
          res += new FileAttachment(
            rs.getString("name"),
            rs.getString("url"),
          )
        }
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def getIssueFileAttachments(id: String): ListBuffer[FileAttachment] ={
    val res = ListBuffer.empty[FileAttachment]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from file_attachments where issue_id = $id")
        while (rs.next()){
          res += new FileAttachment(
            rs.getString("name"),
            rs.getString("url"),
          )
        }
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def setIssueFileAttachments(id: String, file: FileAttachment): Unit ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"insert into file_attachments (issue_id, name, url) values ('$id', '${file.name}', '${file.url}')")
        c.close()
      case _ =>
    }
  }
  def setMessageFileAttachments(id: String, file: FileAttachment): Unit ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"insert into file_attachments (message_id, name, url) values ('$id', '${file.name}', '${file.url}')")
        c.close()
      case _ =>
    }
  }
}
