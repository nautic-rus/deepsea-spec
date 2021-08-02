package deepsea.issues

import akka.actor.Actor
import deepsea.database.DatabaseManager.GetConnection
import deepsea.issues.IssueManager.{GetIssueProjects, GetIssueTypes, IdName}
import play.api.libs.json.{Json, OWrites}

import scala.collection.mutable.ListBuffer

object IssueManager{
  case class GetIssueProjects()
  case class GetIssueTypes()
  case class IdName(id: Int, name: String)
  implicit val writesUser: OWrites[IdName] = Json.writes[IdName]
}
class IssueManager extends Actor{
  override def receive: Receive = {
    case GetIssueProjects() => sender() ! Json.toJson(getIssueProjects)
    case GetIssueTypes() => sender() ! Json.toJson(getIssueTypes)
    case _ => None
  }
  def getIssueProjects: ListBuffer[IdName] ={
    val res = ListBuffer.empty[IdName]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from issue_projects")
        while (rs.next()) {
          res += IdName(
            rs.getInt("id"),
            rs.getString("name"))
        }
        s.close()
        c.close()
        res
      case _ => ListBuffer.empty[IdName]
    }
  }
  def getIssueTypes: ListBuffer[IdName] ={
    val res = ListBuffer.empty[IdName]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from issue_types")
        while (rs.next()) {
          res += IdName(
            rs.getInt("id"),
            rs.getString("name"))
        }
        s.close()
        c.close()
        res
      case _ => ListBuffer.empty[IdName]
    }
  }
}
