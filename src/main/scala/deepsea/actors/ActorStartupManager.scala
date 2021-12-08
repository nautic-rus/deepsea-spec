package deepsea.actors

import akka.actor.{Actor, Props}
import akka.routing.RoundRobinPool
import deepsea.actors.ActorManager.system
import deepsea.actors.ActorStartupManager.{HTTPManagerStarted, Start}
import deepsea.http.HTTPManager
import deepsea.spec.SpecManager


object ActorStartupManager{
  case class Start()
  case class CamundaManagerStarted()
  case class DatabaseManagerStarted()
  case class HTTPManagerStarted()
}
class ActorStartupManager extends Actor{
  override def receive: Receive = {
    case Start() =>
      ActorManager.httpServer = system.actorOf(RoundRobinPool(1).props(Props[HTTPManager]))
    case HTTPManagerStarted() =>
      ActorManager.spec = system.actorOf(RoundRobinPool(10).props(Props[SpecManager]))
  }
}
