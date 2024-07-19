package deepsea.accomodations

import akka.actor.Actor
import akka.pattern.ask
import akka.util.Timeout
import deepsea.accomodations.AccommodationManager.{AddAccommodationGroup, GetAccomUserIdReplace, GetAccommodationsESP, SetAccommodationLabel, UpdateAccommodationUserId}
import deepsea.actors.ActorManager
import deepsea.devices.DeviceManager.Device
import deepsea.files.FileManager.GenerateUrl
import deepsea.pipe.PipeManager.Material
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
import local.common.Codecs
import local.pdf.en.accom.AccomReportEn.genAccomListEnPDF

import java.text.DecimalFormat
import java.util.concurrent.TimeUnit
import scala.concurrent.Await

object AccommodationManager{
  case class GetAccommodations(docNumber: String)
  case class GetAccommodationsESP(docNumber: String, revision: String, lang: String = "en")
  case class BBox(xMin: Double, yMin: Double, zMin: Double, xMax: Double, yMax: Double, zMax: Double)
  case class Zone(name: String, BBox: BBox)
  case class AddAccommodationGroup(docNumber: String, stock: String, userId: String)
  case class SetAccommodationLabel(docNumber: String, userId: String, oid: String)
  case class UpdateAccommodationUserId(docNumber: String, prev: String, next: String)
  case class AccommodationGroup(userId: String, code: String)
  case class GetAccomUserIdReplace(docNumber: String)
  case class AccomUserIdReplace(userId: String, userIdNew: String)
  implicit val AccomUserIdReplaceDecoder: Decoder[AccomUserIdReplace] = deriveDecoder[AccomUserIdReplace]
  implicit val AccomUserIdReplaceEncoder: Encoder[AccomUserIdReplace] = deriveEncoder[AccomUserIdReplace]

  case class Accommodation(project: String, modelOid: Int, asOid: Int, weight: Double, surface: Double, userId: String, materialCode: String, materialDescription: String, objType: Int, pars: List[Double], bsWeight: Double, zone: String, profileStock: String, plateStock: String, var material: Material = Material(), profileLength: Double, profileSection: Int){
    def asDevice: Device ={
      if (profileStock == "COMFSNSTUXXX0030"){
        val q = 0
      }
      Device(
        project,
        modelOid,
        asOid,
        removeLeftZeros(userId),
        "",
        zone,
        "accommodation",
        "",
        if (profileStock != ""){
          if (weight != 0){
            weight
          }
          else {
            profileLength / 1000d * material.singleWeight
          }
        } else{
          if (weight != 0) weight else bsWeight
        },
        material.code,
        0,
        "",
        "",
        "",
//        "",
        if (material.code == "REQPRTGRDNON0001"){
          material.copy(name = material.name + ", " + pars.take(3).map(x => new DecimalFormat("0.#").format(Math.round(x * 1000 * 10) / 10.toDouble)).mkString("x"))
        }
        else if (objType == 67){
          material.copy(name = material.name + ", " + pars.take(4).takeRight(3).map(x => new DecimalFormat("0.#").format(Math.round(x * 1000 * 10) / 10.toDouble)).mkString("x"))
        }
//        else if (objType == 6){
//          material.copy(name = material.name + ", " + pars.take(3).map(x => new DecimalFormat("0.#").format(Math.round(x * 1000 * 10) / 10.toDouble)).mkString("x"))
//        }
        else if (List(23, 68, 69).contains(objType) && pars.length > 4){
          material.copy(name = material.name + ", " + pars.take(5).takeRight(4).map(x => new DecimalFormat("0.#").format(Math.round(x * 1000 * 10) / 10.toDouble)).mkString("x"))
        }
//        else if (objType == 0 && profileLength != 0 && profileLength <= 400 && profileSection != 0){
//          val length = ", L=" + Math.round(profileLength)
//          material.copy(name = material.name + length, translations = material.translations.map(t => t.copy(name = t.name + length)))
//        }
        else{
          material
        },
        "",
        "",
        material.units,
        material.units match {
          case "006" => material.singleWeight
          case "055" => material.singleWeight
          case "796" => 1
          case _ => surface / 2
        }
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
    //case GetAccommodations(docNumber) => sender() ! getAccommodations(docNumber).filter(_.material.code != "").asJson.noSpaces
    case GetAccommodationsESP(docNumber, revision, lang) =>
      val docName: String = getASName(docNumber)
      val devices: List[Device] = getAccommodationsAsDevices(docNumber, lang)
      devices.filter(_.desc2.contains("&")).foreach(d => {
        val ids = d.desc2.split("&")
        val accom = devices.filter(_.elemType == "accommodation").filter(x => ids.contains(x.userId) && x.zone == d.zone)
        accom.foreach(x => x.userId = d.userId + "." + x.userId)
      })
      val userIdsReplace = getAccommodationUserIds(docNumber)
      devices.filter(_.elemType == "accommodation").foreach(x => {
        userIdsReplace.find(_.userId == x.userId) match {
          case Some(value) => x.userId = value.userIdNew
          case _ => None
        }
      })
      val file = genAccomListEnPDF(docNumber, docName, revision, devices, lang)
      Await.result(ActorManager.files ? GenerateUrl(file), timeout.duration) match {
        case url: String => sender() ! url.asJson.noSpaces
        case _ => sender() ! "error".asJson.noSpaces
      }
    case AddAccommodationGroup(docNumber, stock, userId) =>
      addGroupToSystem(docNumber, stock, userId)
      sender() ! "success".asJson.noSpaces
    case SetAccommodationLabel(docNumber, userId, oid) =>
      sender() ! setAccommodationLabel(docNumber, userId, oid.toIntOption.getOrElse(0)).asJson.noSpaces
    case UpdateAccommodationUserId(docNumber, prev, next) =>
      sender() ! updateAccomodationUserId(docNumber, prev, next).asJson.noSpaces
    case GetAccomUserIdReplace(docNumber) =>
      sender() ! getAccommodationUserIds(docNumber).asJson.noSpaces
    case _ => None
  }
}
