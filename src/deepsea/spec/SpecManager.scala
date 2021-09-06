package deepsea.spec

import akka.actor.Actor
import akka.util.Timeout
import deepsea.spec.SpecManager.{GetHullSpec, PartDef}
import play.api.libs.json.{Json, OWrites}

import java.util.concurrent.TimeUnit
import scala.collection.mutable.ListBuffer

object SpecManager{
  case class GetHullSpec(project: String, block: String)
  case class PartDef(name: String, section: String, description: String)
  implicit val writesPartDef: OWrites[PartDef] = Json.writes[PartDef]
}
class SpecManager extends Actor {
  implicit val timeout: Timeout = Timeout(30, TimeUnit.SECONDS)

  override def receive: Receive = {
    case GetHullSpec(project, block) => sender() ! Json.toJson(getHullSpec(project, block))
    case _ => None
  }

  private def getHullSpec(project: String, block: String):  ListBuffer[PartDef] ={
    ListBuffer.empty[PartDef]
  }
}