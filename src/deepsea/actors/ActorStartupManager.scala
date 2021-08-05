package deepsea.actors

import deepsea.actors.ActorManager.system
import deepsea.actors.ActorStartupManager.{CamundaManagerStarted, DatabaseManagerStarted, HTTPManagerStarted, Start}
import akka.actor.{Actor, Props}
import akka.routing.RoundRobinPool
import deepsea.auth.AuthManager
import deepsea.camunda.CamundaManager
import deepsea.database.DatabaseManager
import deepsea.files.FileManager
import deepsea.http.HTTPManager
import deepsea.issues.IssueManager


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
      ActorManager.auth = system.actorOf(RoundRobinPool(5).props(Props[AuthManager]))
      ActorManager.issue = system.actorOf(RoundRobinPool(5).props(Props[IssueManager]))
      ActorManager.files = system.actorOf(RoundRobinPool(5).props(Props[FileManager]))
  }
}
