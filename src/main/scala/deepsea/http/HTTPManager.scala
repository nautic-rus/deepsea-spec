package deepsea.http

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.{Actor, ActorRef}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpEntity, UniversalEntity}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import akka.pattern.ask
import akka.util.Timeout
import deepsea.App
import deepsea.actors.ActorManager
import deepsea.actors.ActorStartupManager.HTTPManagerStarted
import deepsea.hull.HullManager.{GetForanParts, GetForanPartsExcel}
import deepsea.spec.SpecManager.{GetHullBlocks, GetHullPartList, GetHullSpec}
import org.apache.log4j.{LogManager, Logger}
import play.api.libs.json.{JsValue, Json}

import java.util.concurrent.TimeUnit
import scala.concurrent.{Await, ExecutionContextExecutor, Future}

object HTTPManager {
  case class Response(value: String)
}

class HTTPManager extends Actor {
  implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "http")
  implicit val executionContext: ExecutionContextExecutor = system.executionContext
  implicit val timeout: Timeout = Timeout(30, TimeUnit.SECONDS)
  val logger: Logger = LogManager.getLogger("HttpManager")
  var server: Future[Http.ServerBinding] = _
  val routes: Route = {
    concat(
      (get & path("initHullPartList") & parameter("project") & parameter("taskId") & parameter("docNum") & parameter("docName") & parameter("user")) { (project, taskId, docNum, docName, user) =>
        askFor(ActorManager.spec, GetHullSpec(project, taskId, docNum, docName, user))
      },
      (get & path("hullBlocks") & parameter("project")) { (project) =>
        askFor(ActorManager.spec, GetHullBlocks(project))
      },
      (get & path("getHullPartList") & parameter("docNum")) { (docNum) =>
        askFor(ActorManager.spec, GetHullPartList(docNum))
      },
      (get & path("foranPartsExcel") & parameter("project")) { (project) =>
        askFor(ActorManager.hullManager, GetForanPartsExcel(project))
      },


    )
  }

  def askFor(actor: ActorRef, command: Any, long: Boolean = false): StandardRoute = {
    try {
      Await.result(actor ? command, timeout.duration) match {
        case response: JsValue => complete(HttpEntity(response.toString()))
        case response: Array[Byte] => complete(HttpEntity(response))
        case response: String => complete(HttpEntity(response))
        case response: UniversalEntity => complete(response)
        case _ => complete(HttpEntity(Json.toJson("Error: Wrong response from actor.").toString()))
      }
    }
    catch {
      case _: Throwable => complete(HttpEntity(Json.toJson("Error: No response from actor in timeout.").toString()))
    }
  }

  def error: StandardRoute = complete(HttpEntity(Json.toJson("Error: Wrong response.").toString()))

  override def preStart(): Unit = {
    server = Http().newServerAt(App.HTTPServer.Host, App.HTTPServer.Port).bind(routes)
    logger.debug("HTTP server has been started at " + App.HTTPServer.Host + " with port " + App.HTTPServer.Port)
    ActorManager.startup ! HTTPManagerStarted()
  }

  override def postStop(): Unit = {
    server.flatMap(_.unbind()).onComplete(_ => system.terminate())
  }

  override def receive: Receive = {
    case _ => None
  }
}
