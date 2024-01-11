package deepsea.devices

import akka.actor.Actor
import akka.pattern.ask
import akka.util.Timeout
import deepsea.accomodations.AccommodationHelper
import deepsea.actors.ActorManager
import deepsea.devices.DeviceManager.{AddDeviceToSystem, Device, GetDevices, GetDevicesESP, RemoveDeviceFromSystem}
import deepsea.esp.EspManager.GetEsp
import deepsea.files.FileManager.GenerateUrl
import deepsea.pipe.PipeHelper
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
  case class Device(project: String, id: Int, comp: Int, var userId: String, system: String, var zone: String, elemType: String, compAbbrev: String, var weight: Double, stock: String, elemClass: Int, desc1: String, desc2: String, longDesc: String, var material: Material = Material(), origUserId: String, parentUserId: String, var units: String = "796", var count: Double = 1, fromAux: Int = 0)
  case class DeviceAux(id: Int, descr: String)
  case class SystemLang(systemId: Int, lang: Int, descr: String, long_descr: String)

  case class GetDevices(docNumber: String)
  case class GetDevicesESP(docNumber: String, revision: String, lang: String = "en")
  case class AddDeviceToSystem(docNumber: String, stock: String, units: String, count: String, label: String, forLabel: String, addText: String)
  case class RemoveDeviceFromSystem(docNumber: String, stock: String, units: String, count: String, label: String, forLabel: String, addText: String)
}
class DeviceManager extends Actor with DeviceHelper with AccommodationHelper with Codecs {

  implicit val timeout: Timeout = Timeout(60, TimeUnit.SECONDS)

  override def receive: Receive = {
    case GetDevices(docNumber) =>
      val projectSystem = getProjectFromDocNumber(docNumber)
      Await.result(ActorManager.esp ? GetEsp(projectSystem._1.replace("NT02", "N002"), "device", docNumber), timeout.duration) match {
        case res: String => sender() ! res
        case _ => sender() ! "error".asJson.noSpaces
      }

//      val devices = getDevices(docNumber) ++ getAccommodationsAsDevices(docNumber, "ru")
//      devices.filter(_.userId.contains(".")).filter(x => x.zone == "" || x.zone == "-" || x.zone == "*").foreach(x => {
//        devices.find(y => y.userId == x.userId.split("\\.").head) match {
//          case Some(orig) => x.zone = orig.zone
//          case _ => None
//        }
//      })
//      sender() ! devices.asJson.noSpaces
    case GetDevicesESP(docNumber, revision, lang) =>
      val docName: String = getSystemName(docNumber)
      val devices: List[Device] = getDevices(docNumber).tapEach(x => x.userId = removeLeftZeros(x.origUserId)) ++ getAccommodationsAsDevices(docNumber, lang)

//      devices.filter(_.longDescElem.contains("&")).foreach(d => {
//        val ids = d.longDescElem.split("&")
//        val accom = devices.filter(_.elemType == "accommodation").filter(x => ids.contains(x.userId))
//        accom.foreach(x => x.userId = d.userId + "." + x.userId)
//      })

      val rev = if (revision == "NO REV")  "" else revision
      val file = genAccomListEnPDF(docNumber, docName, rev, devices, lang)
      Await.result(ActorManager.files ? GenerateUrl(file), timeout.duration) match {
        case url: String => sender() ! url.asJson.noSpaces
        case _ => sender() ! "error".asJson.noSpaces
      }
    case AddDeviceToSystem(docNumber, stock, units, count, label, forLabel, addText) =>
      addDeviceToSystem(docNumber, stock, units, count, label, forLabel, addText)
      sender() ! "success".asJson.noSpaces
    case RemoveDeviceFromSystem(docNumber, stock, units, count, label, forLabel, addText) =>
      removeDeviceFromSystem(docNumber, stock, units, count, label, forLabel, addText)
      sender() ! "success".asJson.noSpaces
    case _ => None
  }
  def removeLeftZeros(input: String): String ={
    var res = input
    while (res != "" && res.head == '0'){
      res = res.substring(1, res.length)
    }
    res
  }
}
