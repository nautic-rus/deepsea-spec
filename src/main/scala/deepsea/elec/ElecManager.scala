package deepsea.elec

import akka.actor.Actor
import deepsea.elec.ElecManager.{GetCablesByNodes, GetCablesByTray, GetEqLabels, GetTrayLabels, GetTraysByZonesAndSystems}
import local.ele.eq.EleEqManager
import local.ele.trays.TrayManager
import play.api.libs.json.Json

object ElecManager{
  case class GetTrayLabels(project: String, seqId: String)
  case class GetCablesByTray(project: String, seqId: String)
  case class GetEqLabels(project: String, seqId: String)
  case class GetCablesByNodes(project: String, node1: String, node2: String)
  case class GetTraysByZonesAndSystems(project: String, zones: String, systems: String)
}
class ElecManager extends Actor{
  override def receive: Receive = {
    case GetTrayLabels(project, seqId) => sender() ! Json.toJson(TrayManager.trayLabels(project,seqId))
    case GetCablesByTray(project, seqId) => sender() ! Json.toJson(TrayManager.genCablesByTraySeqId(project,seqId))
    case GetEqLabels(project, seqId) => sender() ! Json.toJson(EleEqManager.genEqLabelsByEqOid(project,seqId))
    case GetCablesByNodes(project, node1, node2) => sender() ! Json.toJson(TrayManager.genCablesInLineByTwoNodes(project, node1, node2))
    case GetTraysByZonesAndSystems(project, _zones, _systems) =>
      val zones = _zones.split(",").toList
      val systems = _systems.split(",").toList
      sender() ! Json.toJson(TrayManager.tarysByZonesSystemsJson(project, zones, systems))
    case _ => None
  }
}
