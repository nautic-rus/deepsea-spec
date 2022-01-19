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
import deepsea.elec.ElecManager.{FixTrayBundle, GenerateTrayPdf, GetCablesByNodes, GetCablesByTray, GetEqLabels, GetHoleLabel, GetTrayBundles, GetTrayLabels, GetTraysByZonesAndSystems}
import deepsea.hull.HullManager.{GetHullEsp, GetHullEspFiles, GetHullPart, GetHullPartsByDocNumber, GetHullPartsExcel, SetHullEsp}
import deepsea.spec.SpecManager.{GetHullBlocks, GetHullPartListFromBsTree, SetHullPartListFromBsTree}
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
  implicit val timeout: Timeout = Timeout(300, TimeUnit.SECONDS)
  val logger: Logger = LogManager.getLogger("HttpManager")
  var server: Future[Http.ServerBinding] = _
  val routes: Route = {
    concat(
      //HULL
      (get & path("hullBlocks") & parameter("project")) { (project) =>
        askFor(ActorManager.spec, GetHullBlocks(project))
      },
      (get & path("hullPart") & parameter("project") & parameter("docNumber") & parameter("partCode")) { (project, docNumber, partCode) =>
        askFor(ActorManager.hullManager, GetHullPart(project, docNumber, partCode))
      },

      (get & path("hullPartList") & parameter("project") & parameter("docNumber")) { (project, docNumber) =>
        askFor(ActorManager.hullManager, GetHullPartsByDocNumber(project, docNumber))
      },
      (get & path("hullEsp") & parameter("docNumber") & parameter("revision")) { (docNumber, revision) =>
        askFor(ActorManager.hullManager, GetHullEsp(docNumber, revision))
      },
      (get & path("setHullEsp") & parameter("project") & parameter("docNumber") & parameter("user") & parameter("revision")) { (project, docNumber, user, revision) =>
        askFor(ActorManager.hullManager, SetHullEsp(project, docNumber, user, revision))
      },
      (get & path("hullEspFiles") & parameter("project") & parameter("docNumber") & parameter("revision")) { (project, docNumber, revision) =>
        askFor(ActorManager.hullManager, GetHullEspFiles(project, docNumber, revision))
      },


      (get & path("foranPartsExcel") & parameter("project")) { (project) =>
        askFor(ActorManager.hullManager, GetHullPartsExcel(project))
      },

      //ELEC
      (get & path("trayLabels") & parameter("project") & parameter("seqId")) { (project, seqId) =>
        askFor(ActorManager.elec, GetTrayLabels(project, seqId))
      },
      (get & path("cablesByTray") & parameter("project") & parameter("seqId")) { (project, seqId) =>
        askFor(ActorManager.elec, GetCablesByTray(project, seqId))
      },
      (get & path("cablesByNodes") & parameter("project") & parameter("node1") & parameter("node2")) { (project, node1, node2) =>
        askFor(ActorManager.elec, GetCablesByNodes(project, node1, node2))
      },
      (get & path("eqLabels") & parameter("project") & parameter("seqId")) { (project, seqId) =>
        askFor(ActorManager.elec, GetEqLabels(project, seqId))
      },
      (get & path("elecHoleLabel") & parameter("project") & parameter("seqId")) { (project, seqId) =>
        askFor(ActorManager.elec, GetHoleLabel(project, seqId))
      },
      //ELEC TOOLS
      (get & path("trayBundles") & parameter("project")) { (project) =>
        askFor(ActorManager.elec, GetTrayBundles(project))
      },
      (get & path("traysByZonesAndSystems") & parameter("project") & parameter("docNumber")) { (project, docNumber) =>
        askFor(ActorManager.elec, GetTraysByZonesAndSystems(project, docNumber))
      },
      (get & path("traySpec") & parameter("project") & parameter("docNumber") & parameter("revision")) { (project, docNumber, revision) =>
        askFor(ActorManager.elec, GenerateTrayPdf(project, docNumber, revision))
      },
      (get & path("fixTrayBundle") & parameter("project") & parameter("docNumber")) { (project, docNumber) =>
        askFor(ActorManager.elec, FixTrayBundle(project, docNumber))
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
