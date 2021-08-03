package deepsea.issues

import akka.actor.Actor
import akka.pattern.ask
import akka.util.Timeout
import deepsea.actors.ActorManager
import deepsea.auth.AuthManager.{GetUser, User}
import deepsea.camunda.CamundaManager.{GetIssuesForUser, InitIssueInstance, ProcessIssueInstance}
import deepsea.issues.IssueManager.{GetIssues, InitIssue, IssueDef, ProcessIssue}
import deepsea.issues.classes.Issue
import play.api.libs.json.{Json, OWrites}

import java.util.concurrent.TimeUnit
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await

object IssueManager{
  case class GetIssues(user: String)
  case class InitIssue(user: String)
  case class StartIssue(user: String)
  case class ProcessIssue(issue: String)
  case class GetIssueProjects()
  case class GetIssueTypes()


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
          ActorManager.camunda ! ProcessIssueInstance(issue)
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
    case _ => None
  }
}
