package deepsea.spec

import akka.actor.Actor
import akka.util.Timeout
import deepsea.spec.SpecManager._
import local.hull.BStree
import local.hull.nest.NestManager._
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

  case class GetHullNesting(project: String)
  case class GetHullNestingBlocks(project: String)
  case class GetHullNestingMaterials(project: String, blocks: String)
  case class GetHullNestingByProjectAndBlocks(project: String, blocks: String)

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


    case GetHullNesting(project) =>
      sender() ! genAllPlateNestJson(project)
    case GetHullNestingBlocks(project) =>
      sender() ! genBlocksJson(project)
    case GetHullNestingMaterials(project, blocks) =>
      sender() ! genMaterialNyBlockJson(project, Json.parse(blocks).asOpt[List[String]].getOrElse(List.empty[String]))
    case GetHullNestingByProjectAndBlocks(project, blocks) =>
      sender() ! plateNestByBlocksJson(project, Json.parse(blocks).asOpt[List[String]].getOrElse(List.empty[String]))
    case _ => None
  }


}