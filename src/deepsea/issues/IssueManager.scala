package deepsea.issues

import akka.actor.Actor
import akka.pattern.ask
import akka.util.Timeout
import deepsea.actors.ActorManager
import deepsea.auth.AuthManager.{GetUser, User}
import deepsea.camunda.CamundaManager.{GetIssueInstanceDetails, GetIssuesForUser, InitIssueInstance, ProcessIssueInstance, RemoveIssueInstance}
import deepsea.database.DatabaseManager.GetConnection
import deepsea.issues.IssueManager.{GetIssueDetails, GetIssueMessages, GetIssues, InitIssue, IssueDef, ProcessIssue, RemoveIssue}
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
            case result: String => sender() ! Json.toJson(result)
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
          Await.result(ActorManager.camunda ? GetIssueInstanceDetails(id, user), timeout.duration) match {
            case result: Option[Issue] =>
              result match {
                case Some(issue) => sender() ! Json.toJson(issue)
                case _ => sender() ! Json.toJson("error")
              }
            case _ => sender() ! Json.toJson("error")
          }
        case _ => sender() ! Json.toJson("error")
      }
    case GetIssueMessages(id) => sender() ! getIssueMessages(id)
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
            rs.getLong("date")
          )
        }
        s.close()
        c.close()
      case _ =>
    }
    res
  }
}
