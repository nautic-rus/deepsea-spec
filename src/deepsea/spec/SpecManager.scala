package deepsea.spec

import akka.actor.Actor
import akka.util.Timeout
import deepsea.spec.SpecManager.{GetHullBlocks,  GetHullSpec, GetProjectList, PartDef}
import local.common.Misc
import local.hull.BStree
import play.api.libs.json.{Json, OWrites}

import java.util.concurrent.TimeUnit
import scala.collection.mutable.ListBuffer

object SpecManager {
  case class GetHullSpec(project: String, block: String, taskId: String = "", docNum: String = "", docName: String = "", user: String = "")

  case class GetProjectList()

  case class GetHullBlocks(project: String)

  case class PartDef(name: String, section: String, description: String)

  implicit val writesPartDef: OWrites[PartDef] = Json.writes[PartDef]
}

class SpecManager extends Actor with BStree with Misc {
  implicit val timeout: Timeout = Timeout(30, TimeUnit.SECONDS)

  override def receive: Receive = {
    //Generate part list from BSTREE
    case GetHullSpec(project, block, taskId, docNum, docName, user) => sender() ! genPartList(project, block, taskId, docNum, docName, user)
    //Generate list of projects
    case GetProjectList() => sender() ! genProjectList()
    //Generate list of HULL blocs
    case GetHullBlocks(project) => sender() ! genBlocks(project)

    case _ => None
  }


}