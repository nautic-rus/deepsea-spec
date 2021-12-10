package deepsea.elec

import akka.actor.Actor
import deepsea.elec.ElecManager.{GetCablesByTray, GetEqLabels, GetTrayLabels}
import local.ele.eq.EleEqManager
import local.ele.trays.TrayManager
import play.api.libs.json.Json

object ElecManager{
  case class GetTrayLabels(project: String, seqId: String)
  case class GetCablesByTray(project: String, seqId: String)
  case class GetEqLabels(project: String, seqId: String)
}
class ElecManager extends Actor{
  override def receive: Receive = {
    case GetTrayLabels(project, seqId) => sender() ! Json.toJson(TrayManager.trayLabels(project,seqId))
    case GetCablesByTray(project, seqId) => sender() ! Json.toJson(TrayManager.genCablesByTraySeqId(project,seqId))
    case GetEqLabels(project, seqId) => sender() ! Json.toJson(EleEqManager.genEqLabelsByEqOid(project,seqId))
    case _ => None
  }
}
