package deepsea.actors

import akka.actor.{Actor, Props}
import akka.routing.RoundRobinPool
import deepsea.actors.ActorManager.system
import deepsea.actors.ActorStartupManager.{DatabaseManagerStarted, HTTPManagerStarted, Start}
import deepsea.database.DatabaseManager
import deepsea.elec.ElecManager
import deepsea.files.FileManager
import deepsea.http.HTTPManager
import deepsea.hull.HullManager
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
      ActorManager.dataBase = system.actorOf(Props[DatabaseManager])
    case DatabaseManagerStarted() =>
      ActorManager.httpServer = system.actorOf(RoundRobinPool(1).props(Props[HTTPManager]))
    case HTTPManagerStarted() =>
      ActorManager.spec = system.actorOf(RoundRobinPool(10).props(Props[SpecManager]))
      ActorManager.hullManager = system.actorOf(RoundRobinPool(10).props(Props[HullManager]))
      ActorManager.elec = system.actorOf(RoundRobinPool(10).props(Props[ElecManager]))
      ActorManager.files = system.actorOf(RoundRobinPool(3).props(Props[FileManager]))
  }
}
