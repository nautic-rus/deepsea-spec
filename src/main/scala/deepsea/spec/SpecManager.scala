package deepsea.spec

import akka.actor.Actor
import akka.util.Timeout
import deepsea.actors.ActorManager
import deepsea.files.FileManager.GenerateUrl
import deepsea.spec.SpecManager._
import local.hull.BStree
import local.hull.nest.NestManager._
import play.api.libs.json.{Json, OWrites}
import io.circe._
import io.circe.parser._
import local.hull.nest.CommonNest.NestMaterial

import java.util.concurrent.TimeUnit
import scala.collection.mutable.ListBuffer
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import local.hull.cnc.hyprethermGcode.CNCManager.{doCNC, doCNCStrings}
import local.pdf.ru.ele.EleEqTrayESKDReport.{generatePdfToFileNoRev, generatePdfToFileWithRev}
import akka.pattern.ask
import local.hull.bill.BillManager.{genAnalyticPlateDataJson, genAnalyticProfileDataJson}

import java.nio.file.Files
import scala.concurrent.Await

object SpecManager {

  case class GetHullSpec(project: String, block: String, taskId: String = "", docNum: String = "", docName: String = "", user: String = "")
  case class InitHullPartList(project: String, taskId: String = "", docNum: String = "", docName: String = "", user: String = "")
  case class GetProjectList()
  case class GetHullBlocks(project: String)
  case class GetHullBillPlates(project: String)
  case class GetHullBillProfiles(project: String)

  case class GetHullPartListFromBsTree(project: String, docNum: String)
  case class SetHullPartListFromBsTree(project: String, docNum: String, user: String, revision: String)

  case class GetHullNesting(project: String)
  case class GetHullNestingBlocks(project: String)
  case class GetHullNestingMaterials(project: String, blocks: String)
  case class GetHullNestingByMaterials(project: String, materials: String)

  case class CreateCNC(lines: String, user: String)
  case class InsertNestLock(project: String, nestId: String, user: String)

  case class PartDef(name: String, section: String, description: String)
  implicit val writesPartDef: OWrites[PartDef] = Json.writes[PartDef]
}

class SpecManager extends Actor with BStree {
  implicit val timeout: Timeout = Timeout(30, TimeUnit.SECONDS)
  implicit val nestMaterialDecoder: Decoder[NestMaterial] = deriveDecoder[NestMaterial]

  override def receive: Receive = {

//    //Generate part list from BSTREE
//    case GetHullSpec(project, block, taskId, docNum, docName, user) => sender() ! genPartList(project, block, taskId, docNum, docName, user)
    //Generate list of projects
//    case GetProjectList() => sender() ! genProjectList()
    //Generate list of HULL blocks
    case GetHullBlocks(project) => sender() ! genBlocks(project)



    case GetHullPartListFromBsTree(project, docNum) => sender() ! getHullPartListFromBsTree(project, docNum)
    case SetHullPartListFromBsTree(project, docNum, user, revision) => sender() ! setHullPartListFromBsTree(project, docNum, user, revision)


    case GetHullNesting(project) =>
      sender() ! genAllPlateNestJson(project)
    case GetHullNestingBlocks(project) =>
      sender() ! genBlocksJson(project)
    case GetHullNestingMaterials(project, blocks) =>
      sender() ! genMaterialNyBlockJson(project, Json.parse(blocks).asOpt[List[String]].getOrElse(List.empty[String]))
    case GetHullNestingByMaterials(project, materials) =>
      sender() ! plateNestByMaterialsAndDimsJson(project, decode[List[NestMaterial]](materials).getOrElse(List.empty[NestMaterial]))
    case GetHullBillPlates(project) =>
      sender() ! genAnalyticPlateDataJson(project)
    case GetHullBillProfiles(project) =>
      sender() ! genAnalyticProfileDataJson(project)

    case CreateCNC(lines, user) =>
      val res = doCNCStrings(Json.parse(lines).asOpt[List[String]].getOrElse(List.empty[String]), user)
      sender() ! Json.toJson(res)
    case InsertNestLock(project, nestId, user) =>
      insertLock(project, nestId, user)
      sender() ! Json.toJson("success")

    case _ => None
  }


}