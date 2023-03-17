package deepsea.actors

import akka.actor.{Actor, Props}
import akka.routing.RoundRobinPool
import deepsea.accomodations.AccommodationManager
import deepsea.accomodations.AccommodationManager.Accommodation
import deepsea.actors.ActorManager.system
import deepsea.actors.ActorStartupManager.{DatabaseManagerStarted, HTTPManagerStarted, Start}
import deepsea.database.DatabaseManager
import deepsea.devices.DeviceManager
import deepsea.elec.ElecManager
import deepsea.esp.EspManager
import deepsea.elec.ElecManager.ElecManager
import deepsea.files.FileManager
import deepsea.http.HTTPManager
import deepsea.hull.HullManager
import deepsea.pipe.{PipeCache, PipeManager}
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
      ActorManager.pipeCache = system.actorOf(RoundRobinPool(1).props(Props[PipeCache]))
      ActorManager.pipe = system.actorOf(RoundRobinPool(5).props(Props[PipeManager]))
      ActorManager.devices = system.actorOf(RoundRobinPool(1).props(Props[DeviceManager]))
      ActorManager.accommodations = system.actorOf(RoundRobinPool(1).props(Props[AccommodationManager]))
      ActorManager.esp = system.actorOf(RoundRobinPool(3).props(Props[EspManager]))
  }
}
