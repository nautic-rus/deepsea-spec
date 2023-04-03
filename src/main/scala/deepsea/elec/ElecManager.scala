package deepsea.elec

import akka.actor.Actor
import akka.http.scaladsl.model.{HttpEntity, UniversalEntity}
import akka.http.scaladsl.server.Directives.complete
import akka.pattern.ask
import akka.util.Timeout
import deepsea.App
import deepsea.actors.ActorManager
import deepsea.database.DatabaseManager.GetOracleConnection
import deepsea.elec.ElecManager._
import deepsea.files.FileManager.{CloudFile, CreateFile, GenerateUrl}
import deepsea.pipe.PipeManager.{Material, MaterialTranslation}
import local.ele.CommonEle.{retrieveAllPartsByComplectNameJSON, retrieveEleComplectsJsonString}
import local.ele.cb.CableBoxManager.cableBoxBySeqIdJson
import local.ele.eq.EleEqManager
import local.ele.trays.TrayManager
import local.ele.utils.EleUtils.fixFBS
import local.pdf.ru.ele.EleEqTrayESKDReport.{generatePdfToFileNoRev, generatePdfToFileWithRev}
import play.api.libs.json.{JsValue, Json}

import java.io.{File, FileOutputStream}
import java.nio.file.Files
import java.util.concurrent.TimeUnit
import java.util.{Date, UUID}
import java.util.zip.{ZipEntry, ZipOutputStream}
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import local.pdf.ru.ele.EleEqTrayESKDReport
import local.pdf.ru.ele.EleEqTrayESKDReport.{generatePdfToFileNoRev, generatePdfToFileWithRev}
import local.ele.cl.CableListManager.{cablesByComplectJson, cablesByComplectMagistralVariantJson}
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import local.common.Codecs
import local.pdf.ru.ele.EleTrayCableBoxReportRu.genTraysAndCBListEnPDF

import scala.collection.immutable.Stream.Empty
import scala.io.Source

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

  case class GetTrayBundles(project: String)

  case class GenerateTrayPdf(project: String, docNumber: String, revision: String = "")

  case class FixTrayBundle(project: String, docNumber: String)

  case class GetElecParts(project: String, bundle: String)

  case class GetElecCables(project: String, bundle: String, magistral: String)

  case class ElecCable(cableId: String, fromEq: String, fromEqDescr: String, toEq: String, toEqDescr: String, seg: String, sect: String, spec: String, cabType: String, system: String, systemDescr: String, user: String, fromZone: String, fromZoneDescr: String, toZone: String, toZoneDescr: String, fRout: String)

  case class GetElecInfo(project: String)

  case class GetElecEspFiles(project: String, docNumber: String, docName: String, revision: String)

  case class TrayBySystem(system: String, stockCode: String = "", trayDesc: String = "", length: Double = 0, weight: Double = 0)

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
                     f_rout:  Double = 0,
                     length: Double = 0.0,
                     l_correction: Double = 0,
                     from_system: String = "",
                     from_eq_id: String = "",
                     from_eq_desc: String = "",
                     from_eq: String = "",
                     from_stock_code: String = "",
                     from_x: Double = 0.0,
                     from_y: Double = 0.0,
                     from_z: Double = 0.0,
                     from_zone: String = "",
                     from_zone_desc: String = "",
                     to_system: String = "",
                     to_eq_id: String = "",
                     to_eq_desc: String = "",
                     to_eq: String = "",
                     to_stock_code: String = "",
                     to_x: Double = 0.0,
                     to_y: Double = 0.0,
                     to_z: Double = 0.0,
                     to_zone: String = "",
                     to_zone_desc: String = "",
                     cab_route_area: String = "",
                     stock_code: String = "",
                     material: Material
                   )


  class ElecManager extends Actor with ElecHelper with Codecs {
    implicit val timeout: Timeout = Timeout(60, TimeUnit.SECONDS)

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
        val q = getCablesBySystem(project, docNumber)
        val f = q.asJson.noSpaces
        sender() ! f
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
      case _ => None
    }
  }
}
