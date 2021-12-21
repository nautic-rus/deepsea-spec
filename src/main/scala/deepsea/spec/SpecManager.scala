package deepsea.spec

import akka.actor.Actor
import akka.util.Timeout
import deepsea.spec.SpecManager.{GetHullBlocks, GetHullPartListFromBsTree, GetHullSpec, GetProjectList, SetHullPartListFromBsTree}
import local.hull.BStree
import play.api.libs.json.{Json, OWrites}

import java.util.concurrent.TimeUnit
import scala.collection.mutable.ListBuffer

object SpecManager {

  case class GetHullSpec(project: String, block: String, taskId: String = "", docNum: String = "", docName: String = "", user: String = "")
  case class InitHullPartList(project: String, taskId: String = "", docNum: String = "", docName: String = "", user: String = "")
  case class GetProjectList()
  case class GetHullBlocks(project: String)

  case class GetHullPartListFromBsTree(project: String, docNum: String)
  case class SetHullPartListFromBsTree(project: String, docNum: String, user: String, revision: String)


  case class PartDef(name: String, section: String, description: String)
  implicit val writesPartDef: OWrites[PartDef] = Json.writes[PartDef]
}

class SpecManager extends Actor with BStree {
  implicit val timeout: Timeout = Timeout(30, TimeUnit.SECONDS)

  override def receive: Receive = {

//    //Generate part list from BSTREE
//    case GetHullSpec(project, block, taskId, docNum, docName, user) => sender() ! genPartList(project, block, taskId, docNum, docName, user)
//    //Generate list of projects
//    case GetProjectList() => sender() ! genProjectList()
//    //Generate list of HULL blocks
//    case GetHullBlocks(project) => sender() ! genBlocks(project)



    case GetHullPartListFromBsTree(project, docNum) => sender() ! getHullPartListFromBsTree(project, docNum)
    case SetHullPartListFromBsTree(project, docNum, user, revision) => sender() ! setHullPartListFromBsTree(project, docNum, user, revision)

    case _ => None
  }


}