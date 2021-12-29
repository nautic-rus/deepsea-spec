package deepsea.elec

import akka.actor.Actor
import akka.http.scaladsl.model.{HttpEntity, UniversalEntity}
import akka.http.scaladsl.server.Directives.complete
import akka.pattern.ask
import akka.util.Timeout
import deepsea.App
import deepsea.actors.ActorManager
import deepsea.elec.ElecManager.{GenerateTrayPdf, GetCablesByNodes, GetCablesByTray, GetEqLabels, GetHoleLabel, GetTrayBundles, GetTrayLabels, GetTraysByZonesAndSystems}
import deepsea.files.FileManager.{CloudFile, CreateFile, GenerateUrl}
import local.ele.CommonEle.{retrieveAllPartsByComplectNameJSON, retrieveEleComplectsJsonString}
import local.ele.cb.CableBoxManager.cableBoxBySeqIdJson
import local.ele.eq.EleEqManager
import local.ele.trays.TrayManager
import local.pdf.ru.ele.EleEqTrayESKDReport.{generatePdfToFileNoRev, generatePdfToFileWithRev}
import play.api.libs.json.{JsValue, Json}

import java.io.{File, FileOutputStream}
import java.nio.file.Files
import java.util.concurrent.TimeUnit
import java.util.{Date, UUID}
import java.util.zip.{ZipEntry, ZipOutputStream}
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await

object ElecManager{
  case class GetTrayLabels(project: String, seqId: String)
  case class GetCablesByTray(project: String, seqId: String)
  case class GetEqLabels(project: String, seqId: String)
  case class GetHoleLabel(project: String, seqId: String)
  case class GetCablesByNodes(project: String, node1: String, node2: String)
  case class GetTraysByZonesAndSystems(project: String, docNumber: String)
  case class GetTrayBundles(project: String)
  case class GenerateTrayPdf(project: String, docNumber: String, revision: String = "")

}
class ElecManager extends Actor{
  implicit val timeout: Timeout = Timeout(10, TimeUnit.SECONDS)

  override def receive: Receive = {
    case GetTrayLabels(project, seqId) => sender() ! Json.toJson(TrayManager.trayLabels(project,seqId))
    case GetCablesByTray(project, seqId) => sender() ! Json.toJson(TrayManager.genCablesByTraySeqId(project,seqId))
    case GetEqLabels(project, seqId) => sender() ! Json.toJson(EleEqManager.genEqLabelsByEqOid(project,seqId))
    case GetHoleLabel(project, seqId) => sender() ! cableBoxBySeqIdJson(project,seqId)
    case GetCablesByNodes(project, node1, node2) => sender() ! Json.toJson(TrayManager.genCablesInLineByTwoNodes(project, node1, node2))
    case GetTraysByZonesAndSystems(project, docNumber) =>
      sender() ! retrieveAllPartsByComplectNameJSON(project,docNumber)
    case GetTrayBundles(project) =>
      sender() ! retrieveEleComplectsJsonString(project)


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


    case _ => None
  }
}
