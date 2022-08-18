package deepsea.devices

import akka.actor.Actor
import akka.pattern.ask
import akka.util.Timeout
import deepsea.actors.ActorManager
import deepsea.devices.DeviceManager.{Accommodation, GetDevices, GetDevicesESP}
import deepsea.files.FileManager.GenerateUrl
import deepsea.pipe.PipeManager.Material
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import local.common.Codecs
import io.circe.parser.decode
import local.pdf.en.accom.AccomReportEn.genAccomListEnPDF
import local.pdf.en.pipe.SpoolsReportEN.{genSpoolsListEnPDF, genSpoolsListEnPDFAll}

import java.util.concurrent.TimeUnit
import scala.concurrent.Await

object DeviceManager{
  case class Accommodation(project: String, id: Int, comp: Int, userId: String, system: String, zone: String, elemType: String, compAbbrev: String, weight: Double, stock: String, elemClass: Int, desc1: String, desc2: String, var material: Material = Material(), units: String = "796", count: Double = 1, fromAux: Int = 0)
  case class AccommodationAux(elem: Int, longDescr: String)

  case class GetDevices(docNumber: String)
  case class GetDevicesESP(docNumber: String, revision: String)
}
class DeviceManager extends Actor with DeviceHelper with Codecs{

  implicit val timeout: Timeout = Timeout(60, TimeUnit.SECONDS)

  override def receive: Receive = {
    case GetDevices(docNumber) =>
      sender() ! getAccommodations(docNumber).asJson.noSpaces
    case GetDevicesESP(docNumber, revision) =>
      val docName: String = getSystemName(docNumber)
      val devices: List[Accommodation] = getAccommodations(docNumber)
      val file = genAccomListEnPDF(docNumber, docName, revision, devices)
      Await.result(ActorManager.files ? GenerateUrl(file), timeout.duration) match {
        case url: String => sender() ! url.asJson.noSpaces
        case _ => sender() ! "error".asJson.noSpaces
      }
    case _ => None
  }
}
