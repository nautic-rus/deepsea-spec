package deepsea.accomodations

import akka.actor.Actor
import akka.pattern.ask
import akka.util.Timeout
import deepsea.accomodations.AccommodationManager.{GetAccommodations, GetAccommodationsESP}
import deepsea.actors.ActorManager
import deepsea.devices.DeviceManager.{Device, GetDevicesESP}
import deepsea.files.FileManager.GenerateUrl
import deepsea.pipe.PipeManager.Material
import io.circe.syntax.EncoderOps
import local.common.Codecs
import local.pdf.en.accom.AccomReportEn.genAccomListEnPDF

import java.util.concurrent.TimeUnit
import scala.concurrent.Await

object AccommodationManager{
  case class GetAccommodations(docNumber: String)
  case class GetAccommodationsESP(docNumber: String, revision: String, lang: String = "en")

  case class Accommodation(project: String, modelOid: Int, asOid: Int, weight: Double, surface: Double, userId: String, materialCode: String, materialDescription: String, bsWeight: Double, zone: String, var material: Material = Material()){
    def asDevice: Device ={
      Device(
        project,
        modelOid,
        asOid,
        removeLeftZeros(userId),
        "",
        zone,
        "accommodation",
        "",
        bsWeight,
        material.code,
        0,
        "",
        "",
        material,
        "",
        "",
        material.units,
        bsWeight
      )
    }
    private def removeLeftZeros(input: String): String ={
      var res = input
      while (res != "" && res.head == '0'){
        res = res.substring(1, res.length)
      }
      res
    }
  }
  case class AccommodationAux(id: Int, descr: String)
}
class AccommodationManager extends Actor with AccommodationHelper with Codecs {

  implicit val timeout: Timeout = Timeout(60, TimeUnit.SECONDS)

  override def receive: Receive = {
    case GetAccommodations(docNumber) => sender() ! getAccommodations(docNumber).filter(_.material.code != "").asJson.noSpaces
    case GetAccommodationsESP(docNumber, revision, lang) =>
      val docName: String = getASName(docNumber)
      val devices: List[Device] = getAccommodations(docNumber).filter(_.material.code != "").map(_.asDevice)
      val file = genAccomListEnPDF(docNumber, docName, revision, devices, lang)
      Await.result(ActorManager.files ? GenerateUrl(file), timeout.duration) match {
        case url: String => sender() ! url.asJson.noSpaces
        case _ => sender() ! "error".asJson.noSpaces
      }
    case _ => None
  }
}
