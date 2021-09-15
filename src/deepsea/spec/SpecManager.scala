package deepsea.spec

import akka.actor.Actor
import akka.util.Timeout
import deepsea.spec.SpecManager.GetHullSpec
import local.hull.BStree

import java.util.concurrent.TimeUnit

object SpecManager {
  case class GetHullSpec(project: String, block: String, taskId: String = "", docNum: String = "", docName: String = "", user: String = "")
}

class SpecManager extends Actor with BStree {
  implicit val timeout: Timeout = Timeout(30, TimeUnit.SECONDS)

  override def receive: Receive = {
    case GetHullSpec(project, block, taskId, docNum, docName,user) => sender() ! genPartList(project, block, taskId, docNum, docName,user)
    case _ => None
  }


}