package deepsea.materials

import akka.actor.Actor



object MaterialManager{
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
