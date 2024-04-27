package deepsea.elec

import akka.actor.Actor
import akka.pattern.ask
import akka.util.Timeout
import deepsea.actors.ActorManager
import deepsea.esp.EspManager.CreateEsp
import deepsea.files.FileManager.GenerateUrl
import deepsea.pipe.PipeManager.Material
import io.circe.{Decoder, Encoder}
import io.circe.generic.auto._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._
import local.common.Codecs
import local.common.DBRequests.MountItem
import local.domain.WorkShopMaterial
import local.ele.CommonEle.{retrieveAllPartsByComplectNameJSON, retrieveEleComplectsJsonString}
import local.ele.cb.CableBoxManager.cableBoxBySeqIdJson
import local.ele.cl.CableListManager.{cablesByComplectJson, cablesByComplectMagistralVariantJson}
import local.ele.eq.EleEqManager
import local.ele.trays.TrayManager
import local.ele.utils.EleUtils.fixFBS
import local.pdf.en.accom.AccomReportEn.genAccomListEnPDF
import local.pdf.ru.ele.EleEqTrayESKDReport
import local.pdf.ru.ele.EleEqTrayESKDReport.{generatePdfToFileNoRev, generatePdfToFileWithRev}
import local.pdf.ru.ele.EleTrayCableBoxReportRu.genTraysAndCBListEnPDF
import play.api.libs.json.Json

import java.nio.file.Files
import java.util.concurrent.TimeUnit
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await

object ElecManager {
  case class GetTrayLabels(project: String, seqId: String)

  case class GetCablesByTray(project: String, seqId: String, bundle: String)

  case class GetEqLabels(project: String, seqId: String)

  case class GetHoleLabel(project: String, seqId: String)

  case class GetCablesByNodes(project: String, node1: String, node2: String, bundle: String)

  case class GetTraysByZonesAndSystems(project: String, docNumber: String)

  case class GetTraysBySystem(project: String, docNumber: String)

  case class GetTotalTraysBySystem(project: String, docNumber: String)

  case class GetCableBoxesBySystem(project: String, docNumber: String)

  case class GetCablesBySystem(project: String, docNumber: String)

  case class GetEquipmentsBySystem(project: String, docNumber: String)

  case class GetTrayBundles(project: String)

  case class GenerateTrayPdf(project: String, docNumber: String, revision: String = "")

  case class FixTrayBundle(project: String, docNumber: String)

  case class GetElecParts(project: String, bundle: String)

  case class GetElecCables(project: String, bundle: String, magistral: String)

  case class ElecCable(cableId: String, fromEq: String, fromEqDescr: String, toEq: String, toEqDescr: String, seg: String, sect: String, spec: String, cabType: String, system: String, systemDescr: String, user: String, fromZone: String, fromZoneDescr: String, toZone: String, toZoneDescr: String, fRout: String)

  case class GetElecInfo(project: String)

  case class GetElecEspFiles(project: String, docNumber: String, docName: String, revision: String)

  case class TrayBySystem(system: String, stockCode: String = "", trayDesc: String = "", length: Double = 0, weight: Double = 0)

  case class ElecAngle(name: String, code: String)

  case class TraysBySystem(
                            system: String = "",
                            oid: Int = 0,
                            zone: String = "",
                            line: Int = 0,
                            weight: Double = 0,
                            x_cog: Double = 0,
                            y_cog: Double = 0,
                            z_cog: Double = 0,
                            cType: String = "",
                            tType: Int = 0,
                            stockCode: String = "",
                            trayDesc: String = "",
                            node1: String = "",
                            x_n1: Double = 0,
                            y_n1: Double = 0,
                            z_n1: Double = 0,
                            node2: String = "",
                            x_n2: Double = 0,
                            y_n2: Double = 0,
                            z_n2: Double = 0,
                            length: Double = 0,
                            //                            height: Double = 0,
                            material: Material
                          )

  case class CableBoxesBySystem(
                                 system: String = "",
                                 userId: String = "",
                                 oid: Int = 0,
                                 zone: String = "",
                                 line: Int = 0,
                                 weight: Double = 0,
                                 x_cog: Double = 0,
                                 y_cog: Double = 0,
                                 z_cog: Double = 0,
                                 sType: String = "",
                                 tType: Int = 0,
                                 code: String = "",
                                 stockCode: String = "",
                                 trayDesc: String = "",
                                 node1: String = "",
                                 x_n1: Double = 0,
                                 y_n1: Double = 0,
                                 z_n1: Double = 0,
                                 node2: String = "",
                                 x_n2: Double = 0,
                                 y_n2: Double = 0,
                                 z_n2: Double = 0,
                                 length: Double = 0,
                                 material: Material
                               )

  case class TrayAndCableBox(
                              trays: List[TraysBySystem],
                              cableBoxes: List[CableBoxesBySystem],
                            )

  case class CableRoute(
                         system: String = "",
                         code: String = "",
                         description: String = "",
                         nom_section: String = "",
                         diameter: Int = 0,
                         seg_code: String,
                         bunch: String,
                         f_rout: Double = 0,
                         length: Double = 0.0,
                         ext_len_1: Double = 0,
                         ext_len_2: Double = 0,
                         from_system: String = "",
                         from_eq_id: Int = 0,
                         from_eq_desc: String = "",
                         from_eq: String = "",
                         from_stock_code: String = "",
                         from_x: Double = 0.0,
                         from_y: Double = 0.0,
                         from_z: Double = 0.0,
                         from_zone: String = "",
                         from_zone_desc: String = "",
                         to_system: String = "",
                         to_eq_id: Int = 0,
                         to_eq_desc: String = "",
                         to_eq: String = "",
                         to_stock_code: String = "",
                         to_x: Double = 0.0,
                         to_y: Double = 0.0,
                         to_z: Double = 0.0,
                         to_zone: String = "",
                         to_zone_desc: String = "",
                         cab_route_area: List[String],
                         cab_route_area_id: List[Int],
                         stock_code: String = "",
                         material: Material
                       )

  case class NodeConnect(
                          id: Int,
                          name: String,
                          count: Int
                        )

  case class EquipmentConnection(
                                  OID: Int = 0,
                                  LABEL: String = "",
                                  TYPE: Int = 0,
                                  USERID: String = "",
                                  NODE_USERID: String = "",
                                  ZONE_SEQID: Int = 0,
                                  ZONE_NAME: String = "",
                                  ZONE_DESCR: String = "",
                                  SYSTEM_SEQID: Int = 0,
                                  SYSTEM_NAME: String = "",
                                  SYSTEM_DESCR: String = "",
                                  ABBREV: String = "",
                                  XCOG: Double = 0.0,
                                  YCOG: Double = 0.0,
                                  ZCOG: Double = 0.0,
                                  PX: Double = 0.0,
                                  PY: Double = 0.0,
                                  PZ: Double = 0.0,
                                  WEIGHT: Double = 0.0,
                                  STOCK_CODE: String = "",
                                  CLASS_NAME: String = "",
                                  SURFACE: String = "",
                                  SUPPORTS: List[MountItem] = List.empty[MountItem],
                                  workShopMaterial: WorkShopMaterial = new WorkShopMaterial()
                                )
  case class ForanEq(
                              OID: Int,
                              TYPE: Int,
                              USERID: String,
                              ZONE_SEQID: Int,
                              ZONE_NAME: String,
                              ZONE_DESCR: String,
                              SYSTEM_SEQID: Int,
                              SYSTEM_NAME: String,
                              SYSTEM_DESCR: String,
                              ABBREV: String,
                              WEIGHT: Double,
                              STOCK_CODE: String,
                              CLASS_NAME: String,
                              RA_CODE: String = "",
                              RA_DESCR: String = "",
                              NODE_USERID: String = "",
                              EQELEC: String,
                              XCOG: Double,
                              YCOG: Double,
                              ZCOG: Double,
                              A11: Double,
                              A12: Double,
                              A13: Double,
                              A21: Double,
                              A22: Double,
                              A23: Double,
                              A31: Double,
                              A32: Double,
                              A33: Double,
                              A41: Double,
                              A42: Double,
                              A43: Double,
                              PX: Double,
                              PY: Double,
                              PZ: Double,
                              SURFACE: String)

  case class EleEq(

                  )

  case class GetElecBlocks(project: String)
  case class GetElecZones(project: String)
  case class GetElecSystems(project: String)
  case class GetEleComplects(project: String)
  case class GetElecProjects()
  case class AddEleComplect(complect: String)
  case class DeleteEleComplect(drawing: String)
  case class UpdateEleComplect(drawing: String)
  case class Block(code: String, name: String)
  case class Zone(code: String, name: String)
  case class System(code: String, name: String)

  implicit val BlockDecoder: Decoder[Block] = deriveDecoder[Block]
  implicit val BlockEncoder: Encoder[Block] = deriveEncoder[Block]

  implicit val ZoneDecoder: Decoder[Zone] = deriveDecoder[Zone]
  implicit val ZoneEncoder: Encoder[Zone] = deriveEncoder[Zone]

  implicit val SystemDecoder: Decoder[System] = deriveDecoder[System]
  implicit val SystemEncoder: Encoder[System] = deriveEncoder[System]


  case class EleTray(userId: String, stock: String, weight: Double, cType: String, idsq: Int, node1: Int, node2: Int, kind: String, cog: Cog, zone: String, material: Material)
  case class EleEquip(userId: String, abbrev: String, stock: String, weight: Double, cog: Cog, zone: String, material: Material)
  case class EleElement(userId: String, typeName: String, units: String, weight: Double, code: String, material: Material, cog: Cog, zone: String, count: Double)
  case class Cog(x: Double, y: Double, z: Double)

  case class ElePos(project: String, kind: String, stock: String, label: String, node: String, descr: String)
  implicit val ElecPosDecoder: Decoder[ElePos] = deriveDecoder[ElePos]
  implicit val ElecPosEncoder: Encoder[ElePos] = deriveEncoder[ElePos]

  case class GetEleEspFiles(foranProject: String, docNumber: String, rev: String, user: String, taskId: String)
  case class GetEleCurrent(foranProject: String, docNumber: String, rev: String, user: String, taskId: String)

  case class GetElePos(project: String, index: String, kind: String, taskId: String)

  case class MaterialLabel(code: String, label: String)

  class ElecManager extends Actor with ElecHelper with Codecs with ElePdf {
    implicit val timeout: Timeout = Timeout(60, TimeUnit.SECONDS)

    override def preStart(): Unit = {

    }

    override def receive: Receive = {
      case GetTrayLabels(project, seqId) => sender() ! Json.toJson(TrayManager.trayLabels(project, seqId))
      case GetCablesByTray(project, seqId, bundle) => sender() ! Json.toJson(TrayManager.genCablesByTraySeqIdAndComplect(project, seqId, bundle))
      case GetEqLabels(project, seqId) => sender() ! Json.toJson(EleEqManager.genEqLabelsByEqOid(project, seqId))
      case GetHoleLabel(project, seqId) => sender() ! cableBoxBySeqIdJson(project, seqId)
      case GetCablesByNodes(project, node1, node2, bundle) => sender() ! Json.toJson(TrayManager.genCablesInLineByTwoNodesAndComplect(project, node1, node2, bundle))
      case GetTraysByZonesAndSystems(project, docNumber) =>
        sender() ! retrieveAllPartsByComplectNameJSON(project, docNumber)
      case GetTraysBySystem(project, docNumber) =>
        sender() ! getTraysBySystem(project, docNumber).asJson.noSpaces
      case GetTotalTraysBySystem(project, docNumber) =>
        sender() ! getTotalTraysBySystem(project, docNumber).asJson.noSpaces
      case GetCableBoxesBySystem(project, docNumber) =>
        val q = getCableBoxesBySystem(project, docNumber)
        val f = q.asJson.noSpaces
        sender() ! f
      case GetCablesBySystem(project, docNumber) =>
        sender() ! getCablesBySystem(project, docNumber).asJson.noSpaces
      case GetEquipmentsBySystem(project, docNumber) =>
        sender() ! getEquipmentsBySystem(project, docNumber).asJson.noSpaces
      case GetTrayBundles(project) =>
        sender() ! retrieveEleComplectsJsonString(project)
      case FixTrayBundle(project, docNumber) =>
        sender() ! fixFBS(project, docNumber)
      case GenerateTrayPdf(project, docNumber, revision) =>
        val tempDirectory = Files.createTempDirectory("trayPdf").toAbsolutePath.toString
        val files = revision match {
          case "" => generatePdfToFileNoRev(project, docNumber, tempDirectory)
          case _ => generatePdfToFileWithRev(project, docNumber, tempDirectory, revision)
        }
        val res = ListBuffer.empty[String]
        files.foreach(file => {
          Await.result(ActorManager.files ? GenerateUrl(file), timeout.duration) match {
            case url: String =>
              res += url
            case _ => None
          }
        })
        sender() ! Json.toJson(res)
      case GetElecParts(project, bundle) =>
        sender() ! EleEqTrayESKDReport.generateElecPartsTojson(project, bundle)
      case GetElecCables(project, bundle, magistral) =>
        //"P701","170701-884-5007"
        sender() ! (magistral.toIntOption.getOrElse(0) match {
          case 0 => cablesByComplectJson(project, bundle)
          case 1 => cablesByComplectMagistralVariantJson(project, bundle)
        })
      case GetElecInfo(project) =>
        sender() ! getCablesInfo(project).asJson.noSpaces
      case GetElecEspFiles(project, docNumber, docName, revision) =>
        val rev = if (revision == "NO REV") "" else revision
        val path = genTraysAndCBListEnPDF(project, docNumber, docName, rev, "ru")
        Await.result(ActorManager.files ? GenerateUrl(path), timeout.duration) match {
          case url: String => sender() ! url.asJson.noSpaces
          case _ => sender() ! "error".asJson.noSpaces
        }
      case GetElecProjects() =>
        sender() ! getElecProjects.asJson.noSpaces
      case GetElecBlocks(project) =>
        sender() ! getBlocks(project).asJson.noSpaces
      case GetElecZones(project) =>
        sender() ! getElecZones(project).asJson.noSpaces
      case GetElecSystems(project) =>
        sender() ! getElecSystems(project).asJson.noSpaces
      case GetEleComplects(project) =>
        sender() ! getEleComplects(project).asJson.noSpaces
      case AddEleComplect(complect: String) =>
        addEleComplect(complect)
        sender() ! "success".asJson.noSpaces
      case DeleteEleComplect(drawing: String) =>
        deleteEleComplect(drawing)
        sender() ! "success".asJson.noSpaces
      case UpdateEleComplect(drawing) =>
        updateEleComplect(drawing)
        sender() ! "success".asJson.noSpaces
      case GetEleEspFiles(foranProject, docNumber, rev, user, taskId) =>
        val taskName = getIssueName(taskId.toIntOption.getOrElse(0))
        val eleEsp = generateEleEsp(foranProject, docNumber, rev, user, taskId)
        val file = genElePdf(eleEsp, taskName)
        Await.result(ActorManager.files ? GenerateUrl(file), timeout.duration) match {
          case url: String => sender() ! url.asJson.noSpaces
          case _ => sender() ! "error: cant upload esp".asJson.noSpaces
        }
      case GetEleCurrent(foranProject, docNumber, rev, user, taskId) =>
        sender() ! generateEleEsp(foranProject, docNumber, rev, user, taskId).asJson.noSpaces
      case GetElePos(project, index, kind, taskId) =>
        sender() ! getElePos(project, kind, index.toIntOption.getOrElse(0), taskId.toIntOption.getOrElse(0)).asJson.noSpaces
      case _ => None
    }
  }
}
