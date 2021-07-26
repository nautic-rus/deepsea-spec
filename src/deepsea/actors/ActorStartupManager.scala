package deepsea.actors

import deepsea.actors.ActorManager.system
import deepsea.actors.ActorStartupManager.{CamundaManagerStarted, DatabaseManagerStarted, HTTPManagerStarted, Start}
import akka.actor.{Actor, Props}
import deepsea.auth.AuthManager
import deepsea.camunda.CamundaManager
import deepsea.database.DatabaseManager
import deepsea.http.HTTPManager


object ActorStartupManager{
  case class Start()
  case class CamundaManagerStarted()
  case class DatabaseManagerStarted()
  case class HTTPManagerStarted()
}
class ActorStartupManager extends Actor{
  override def receive: Receive = {
    case Start() =>
      ActorManager.camunda = system.actorOf(Props[CamundaManager])
    case CamundaManagerStarted() =>
      ActorManager.dataBase = system.actorOf(Props[DatabaseManager])
    case DatabaseManagerStarted() =>
      ActorManager.httpServer = system.actorOf(Props[HTTPManager])
    case HTTPManagerStarted() =>
      ActorManager.auth = system.actorOf(Props[AuthManager])
  }
}
