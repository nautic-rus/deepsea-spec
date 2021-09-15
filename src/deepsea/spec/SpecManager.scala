package deepsea.spec

import akka.actor.Actor
import akka.util.Timeout
import deepsea.spec.SpecManager.{GetHullSpec, PartDef}
import local.hull.BStree
import play.api.libs.json.{Json, OWrites}

import java.util.concurrent.TimeUnit
import scala.collection.mutable.ListBuffer

object SpecManager {
  case class GetHullSpec(project: String, block: String, taskId: String = "", docNum: String = "", docName: String = "", user: String = "")

  case class PartDef(name: String, section: String, description: String)

  implicit val writesPartDef: OWrites[PartDef] = Json.writes[PartDef]
}

class SpecManager extends Actor with BStree {
  implicit val timeout: Timeout = Timeout(30, TimeUnit.SECONDS)

  override def receive: Receive = {
    case GetHullSpec(project, block, taskId, docNum, docName,user) => sender() ! genPartList(project, block, taskId, docNum, docName,user)
    case _ => None
  }


}