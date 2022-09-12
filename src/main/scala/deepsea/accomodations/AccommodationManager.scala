package deepsea.accomodations

import akka.actor.Actor
import deepsea.accomodations.AccommodationManager.GetAccommodations
import deepsea.devices.DeviceManager.Device
import deepsea.pipe.PipeManager.Material
import io.circe.syntax.EncoderOps
import local.common.Codecs

object AccommodationManager{
  case class GetAccommodations(docNumber: String)

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
  override def receive: Receive = {
    case GetAccommodations(docNumber) => sender() ! getAccommodations(docNumber).filter(_.material.code != "").asJson.noSpaces
    case _ => None
  }
}
