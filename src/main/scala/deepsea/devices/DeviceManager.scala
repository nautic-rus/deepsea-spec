package deepsea.devices

import akka.actor.Actor
import deepsea.devices.DeviceManager.GetDevices
import deepsea.pipe.PipeManager.Material
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import local.common.Codecs
import io.circe.parser.decode

object DeviceManager{
  case class Device(project: String, id: Int, comp: Int, userId: String, system: String, zone: String, elemType: String, compAbbrev: String, weight: Double, stock: String, elemClass: Int, desc1: String, desc2: String, var material: Material = Material(), units: String = "796", count: Double = 1, fromAux: Int = 0)
  case class DeviceAux(elem: Int, longDescr: String)

  case class GetDevices(docNumber: String)
}
class DeviceManager extends Actor with DeviceHelper with Codecs{

  override def receive: Receive = {
    case GetDevices(docNumber) =>
      sender() ! getDevices(docNumber).asJson.noSpaces
    case _ => None
  }
}
