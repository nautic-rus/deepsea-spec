package deepsea.materials

import akka.actor.Actor
import deepsea.esp.EspManager.{GlobalEsp, GlobalEspSpec}
import deepsea.esp.EspManagerHelper

import scala.collection.mutable.ListBuffer



object MaterialManager extends MaterialsHelper{
  case class Material(stock: String, name: String, desc: String, units: String, weight: Double, supplier: String, stmt_id: Int, dir_id: Int, user_id: Int, label: String, last_upd: Long, note: String, manufacturer: String)

}
class MaterialManager {
  case class GetMaterials()

  class MaterialManager extends Actor with MaterialHelper {
    override def receive: Receive = {

      case _ => None
    }
  }

}
