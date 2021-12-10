package deepsea.actors

import deepsea.actors.ActorStartupManager.Start
import akka.actor.{ActorRef, ActorSystem, Props}
import deepsea.elec.ElecManager

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object ActorManager {

  var system: ActorSystem = _
  var startup: ActorRef = _
  var httpServer: ActorRef = _
  var spec: ActorRef = _
  var hullManager: ActorRef = _
  var dataBase: ActorRef = _
  var elec: ActorRef = _

  def init(): Unit ={
    system = ActorSystem()
    startup = system.actorOf(Props[ActorStartupManager])
    startup ! Start()
  }
  def initSchedulers(): Unit ={

  }
  def terminate(): Unit ={
    system.terminate()
    Await.ready(system.whenTerminated, Duration(30, TimeUnit.SECONDS))
  }
}
