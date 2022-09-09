package deepsea.accomodations

import akka.actor.Actor
import deepsea.accomodations.AccommodationManager.GetAccommodations
import deepsea.pipe.PipeManager.Material
import io.circe.syntax.EncoderOps
import local.common.Codecs

object AccommodationManager{
  case class GetAccommodations(docNumber: String)

  case class Accommodation(project: String, modelOid: Int, asOid: Int, weight: Double, surface: Double, userId: String, materialCode: String, materialDescription: String, bsWeight: Double, zone: String, var material: Material = Material())
  case class AccommodationAux(id: Int, descr: String)
}
class AccommodationManager extends Actor with AccommodationHelper with Codecs {
  override def receive: Receive = {
    case GetAccommodations(docNumber) => getAccommodations(docNumber).asJson.noSpaces
    case _ => None
  }
}
