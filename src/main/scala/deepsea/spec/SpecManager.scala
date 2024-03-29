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
import io.circe.syntax.EncoderOps
import local.eqmount.EqFoundationManager
import local.eqmount.EqFoundationManager.foranFoudationsAndEqsJson
import local.hull.bill.BillManager.{genAnalyticPlateDataJson, genAnalyticProfileDataJson, genProfileNestBillJson, genWastsgeByParentKplJson}
import local.hull.cnc.pellaESSI.EssiCNCManagerSubdiv.doEssiCNC
import local.qrutil.QrDxfHelper

import java.io.{File, FileWriter}
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
  case class GetHullNestingByProjectPlates(project: String)
  case class GetHullNestingByProjectProfiles(project: String)
  case class GetHullBillPlatesWastage(project: String, kpl: String)

  case class CreateCNC(lines: String, user: String)
  case class CreateESSI(lines: String, user: String)
  case class CreateTAP(lines: String, user: String)
  case class InsertNestLock(project: String, nestId: String, user: String)

  case class GenerateQRCode(url: String)
  case class GetEqFoundations(project: String)
  case class UpdateStatusEqFoundations(project: String, id: String, user: String)

  case class PartDef(name: String, section: String, description: String)
  implicit val writesPartDef: OWrites[PartDef] = Json.writes[PartDef]
}

class SpecManager extends Actor with BStree with QrDxfHelper{
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
    case GetHullNestingByProjectPlates(project) =>
      sender() ! genAllPlateNest2Json(project)
    case GetHullNestingByProjectProfiles(project) =>
      sender() ! genProfileNestBillJson(project)
    case GetHullBillPlates(project) =>
      sender() ! genAnalyticPlateDataJson(project)
    case GetHullBillProfiles(project) =>
      sender() ! genAnalyticProfileDataJson(project)
    case GetHullBillPlatesWastage(project, kpl) =>
      sender() ! genWastsgeByParentKplJson(project, kpl.toIntOption.getOrElse(0))

    case CreateCNC(lines, user) =>
      val res = local.hull.cnc.hyprethermGcode.CNCManager.doCNCStrings(Json.parse(lines).asOpt[List[String]].getOrElse(List.empty[String]), user)
      sender() ! Json.toJson(res)
    case CreateESSI(lines, user) =>
      val res = doEssiCNC(Json.parse(lines).asOpt[List[String]].getOrElse(List.empty[String]), user)
      sender() ! Json.toJson(res)
    case CreateTAP(lines, user) =>
      val res = local.hull.cnc.gcodePella.CNCManager.doCNCStrings(Json.parse(lines).asOpt[List[String]].getOrElse(List.empty[String]), user)
      sender() ! Json.toJson(res)
    case InsertNestLock(project, nestId, user) =>
      insertLock(project, nestId, user)
      sender() ! Json.toJson("success")
    case GetEqFoundations(project) =>
      sender() ! foranFoudationsAndEqsJson(project)
    case UpdateStatusEqFoundations(project, id, user) =>
      sender() ! (EqFoundationManager.updateStatusSQL(project, id.toIntOption.getOrElse(0), user) match {
        case 1 => "success".asJson.noSpaces
        case _ => "error".asJson.noSpaces
      })

    case GenerateQRCode(url) =>
      val str: String = url2qrDXF(url)
      val file = File.createTempFile("qr-code", ".dwg")
      val fileWriter = new FileWriter(file)
      fileWriter.write(str)
      fileWriter.flush()
      fileWriter.close()
      sender() ! file

    case _ => None
  }


}