package deepsea.spec

import akka.actor.Actor
import akka.util.Timeout
import deepsea.spec.SpecManager.{GetHullPartListFromBsTree, SetHullPartListFromBsTree}
import io.circe.syntax.EncoderOps
import local.common.Misc
import local.hull.BStree
import play.api.libs.json.{Json, OWrites}
import java.util.concurrent.TimeUnit

object SpecManager {

  case class GetHullSpec(project: String, block: String, taskId: String = "", docNum: String = "", docName: String = "", user: String = "")
  case class InitHullPartList(project: String, taskId: String = "", docNum: String = "", docName: String = "", user: String = "")
  case class GetProjectList()
  case class GetHullBlocks(project: String)

  case class GetHullPartListFromBsTree(project: String, docNum: String)
  case class SetHullPartListFromBsTree(project: String, docNum: String, user: String, revision: String)


  case class PartDef(name: String, section: String, description: String)
  implicit val writesPartDef: OWrites[PartDef] = Json.writes[PartDef]
}

class SpecManager extends Actor with BStree with Misc {
  implicit val timeout: Timeout = Timeout(30, TimeUnit.SECONDS)

  override def receive: Receive = {




    case GetHullPartListFromBsTree(project, docNum) =>
      getHullPartListFromBsTree(project, docNum) match {
        case Some(value) => sender() ! value.asJson.noSpaces
        case _ => None
      }
    case SetHullPartListFromBsTree(project, docNum, user, revision) => sender() ! setHullPartListFromBsTree(project, docNum, user, revision)

    case _ => None
  }


}