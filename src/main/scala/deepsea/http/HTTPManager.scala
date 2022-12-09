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
import deepsea.elec.ElecManager._
import deepsea.hull.HullManager.{GetBsDesignNodes, GetHullEsp, GetHullEspFiles, GetHullPart, GetHullPartsByDocNumber, GetHullPartsExcel, GetHullPlatesForMaterial, GetHullProfilesForMaterial, GetHullSystems, RemoveParts, SetHullEsp}
import deepsea.spec.SpecManager._
import org.apache.log4j.{LogManager, Logger}
import play.api.libs.json.{JsValue, Json}
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors
import deepsea.accomodations.AccommodationManager.{AddAccommodationGroup, GetAccommodations, GetAccommodationsESP, SetAccommodationLabel}
import deepsea.devices.DeviceManager.{AddDeviceToSystem, GetDevices, GetDevicesESP, RemoveDeviceFromSystem}
import deepsea.pipe.PipeManager.{GetPipeESP, GetPipeSegs, GetPipeSegsBilling, GetPipeSegsByDocNumber, GetSpoolLocks, GetSpoolModel, GetSystems, GetZones, SetSpoolLock}

import java.io.File
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
  val routes: Route = cors() {
    concat(
      //HULL
      (get & path("hullBlocks") & parameter("project")) { (project) =>
        askFor(ActorManager.spec, GetHullBlocks(project))
      },
      (get & path("hullPart") & parameter("project") & parameter("docNumber") & parameter("partCode")) { (project, docNumber, partCode) =>
        askFor(ActorManager.hullManager, GetHullPart(project, docNumber, partCode))
      },
      (get & path("hullPlates") & parameter("project") & parameter("material") & parameter("thickness")) { (project, material, thickness) =>
        askFor(ActorManager.hullManager, GetHullPlatesForMaterial(project, material, thickness))
      },
      (get & path("hullProfiles") & parameter("project") & parameter("material") & parameter("kse")) { (project, material, kse) =>
        askFor(ActorManager.hullManager, GetHullProfilesForMaterial(project, material, kse))
      },
      (get & path("bsDesignNodes") & parameter("project")) { (project) =>
        askFor(ActorManager.hullManager, GetBsDesignNodes(project))
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
      (get & path("hullEspFiles") & parameter("project") & parameter("docNumber") & parameter("docName") & parameter("revision")) { (project, docNumber, docName, revision) =>
        askFor(ActorManager.hullManager, GetHullEspFiles(project, docNumber, docName, revision))
      },
      (get & path("hullSystems") & parameter("project")) { (project) =>
        askFor(ActorManager.hullManager, GetHullSystems(project))
      },

      (get & path("foranPartsExcel") & parameter("project")) { (project) =>
        askFor(ActorManager.hullManager, GetHullPartsExcel(project))
      },

      //ELEC
      (get & path("trayLabels") & parameter("project") & parameter("seqId")) { (project, seqId) =>
        askFor(ActorManager.elec, GetTrayLabels(project, seqId))
      },
      (get & path("cablesByTray") & parameter("project") & parameter("seqId") & parameter("bundle")) { (project, seqId, bundle) =>
        askFor(ActorManager.elec, GetCablesByTray(project, seqId, bundle))
      },
      (get & path("cablesByNodes") & parameter("project") & parameter("node1") & parameter("node2") & parameter("bundle")) { (project, node1, node2, bundle) =>
        askFor(ActorManager.elec, GetCablesByNodes(project, node1, node2, bundle))
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
      (get & path("elecParts") & parameter("project") & parameter("bundle")) { (project, bundle) =>
        askFor(ActorManager.elec, GetElecParts(project, bundle))
      },
      (get & path("elecCables") & parameter("project") & parameter("bundle") & parameter("magistral")) { (project, bundle, magistral) =>
        askFor(ActorManager.elec, GetElecCables(project, bundle, magistral))
      },

      (get & path("hullNesting") & parameter("project")) { (project) =>
        askFor(ActorManager.spec, GetHullNesting(project))
      },
      (get & path("hullNestingBlocks") & parameter("project")) { (project) =>
        askFor(ActorManager.spec, GetHullNestingBlocks(project))
      },
      (post & path("hullNestingMaterials") & entity(as[String]) & parameter("project")) { (blocks, project) =>
        askFor(ActorManager.spec, GetHullNestingMaterials(project, blocks))
      },
      (post & path("hullNestingByMaterials") & entity(as[String]) & parameter("project")) { (materials, project) =>
        askFor(ActorManager.spec, GetHullNestingByMaterials(project, materials))
      },
      (get & path("hullNestingByProjectPlates") & parameter("project")) { (project) =>
        askFor(ActorManager.spec, GetHullNestingByProjectPlates(project))
      },
      (get & path("hullNestingByProjectProfiles") & parameter("project")) { (project) =>
        askFor(ActorManager.spec, GetHullNestingByProjectProfiles(project))
      },
      (get & path("hullBillPlates") & parameter("project")) { (project) =>
        askFor(ActorManager.spec, GetHullBillPlates(project))
      },
      (get & path("hullBillProfiles") & parameter("project")) { (project) =>
        askFor(ActorManager.spec, GetHullBillProfiles(project))
      },
      (get & path("insertNestLock") & parameter("project") & parameter("nestId") & parameter("user")) { (project, nestId, user) =>
        askFor(ActorManager.spec, InsertNestLock(project, nestId, user))
      },
      (get & path("hullPlatesWastage") & parameter("project") & parameter("kpl")) { (project, kpl) =>
        askFor(ActorManager.spec, GetHullBillPlatesWastage(project, kpl))
      },
      (post & path("createCNC") & entity(as[String]) & parameter("user")) { (lines, user) =>
        askFor(ActorManager.spec, CreateCNC(lines, user))
      },
      (post & path("createESSI") & entity(as[String]) & parameter("user")) { (lines, user) =>
        askFor(ActorManager.spec, CreateESSI(lines, user))
      },
      (post & path("createTAP") & entity(as[String]) & parameter("user")) { (lines, user) =>
        askFor(ActorManager.spec, CreateTAP(lines, user))
      },
      (get & path("removeParts") & parameter("project") & parameter("block") & parameter("parts") & parameter("user")){ (project, block, parts, user) =>
        askFor(ActorManager.hullManager, RemoveParts(project, block, parts, user))
      },

      //PIPE
      (get & path("pipeSystems") & parameter("project")) { (project) =>
        askFor(ActorManager.pipe, GetSystems(project))
      },
      (get & path("pipeZones") & parameter("project")) { (project) =>
        askFor(ActorManager.pipe, GetZones(project))
      },
      (get & path("pipeSegs") & parameter("project") & parameter("system") & parameter("sqInSystem")) { (project, system, sqInSystem) =>
        askFor(ActorManager.pipe, GetPipeSegs(project, system, sqInSystem))
      },
      (get & path("pipeSegs") & parameter("project") & parameter("system")) { (project, system) =>
        askFor(ActorManager.pipe, GetPipeSegs(project, system))
      },
      (get & path("pipeSegsByProject") & parameter("project")) { (project) =>
        askFor(ActorManager.pipe, GetPipeSegs(project))
      },
      (get & path("pipeSegs") & parameter("docNumber")) { (docNumber) =>
        askFor(ActorManager.pipe, GetPipeSegsByDocNumber(docNumber))
      },
      (get & path("pipeSegs") & parameter("project")) { (project) =>
        askFor(ActorManager.pipe, GetPipeSegsBilling(project))
      },
      (get & path("spoolLocks") & parameter("docNumber")) { (docNumber) =>
        askFor(ActorManager.pipe, GetSpoolLocks(docNumber))
      },
      (post & path("setSpoolLock") & entity(as[String])) { (jsValue) =>
        askFor(ActorManager.pipe, SetSpoolLock(jsValue))
      },
      (get & path("pipeEspFiles") & parameter("docNumber") & parameter("revision") & parameter("bySpool") & parameter("lang")) { (docNumber, revision, bySpool, lang) =>
        askFor(ActorManager.pipe, GetPipeESP(docNumber, revision, bySpool, lang))
      },
      (get & path("spoolFiles") & parameter("docNumber") & parameter("spool") & parameter("isom")) { (docNumber, spool, isom) =>
        askFor(ActorManager.pipe, GetSpoolModel(docNumber, spool, isom))
      },

      //DEVICES
      (get & path("devices") & parameter("docNumber")) { (docNumber) =>
        askFor(ActorManager.devices, GetDevices(docNumber))
      },
      (get & path("devicesEspFiles") & parameter("docNumber") & parameter("revision") & parameter("lang")) { (docNumber, revision, lang) =>
        askFor(ActorManager.devices, GetDevicesESP(docNumber, revision, lang))
      },
      (get & path("addDeviceToSystem") & parameter("docNumber") & parameter("stock") & parameter("units") & parameter("count") & parameter("label") & parameter("forLabel")) { (docNumber, stock, units, count, label, forLabel) =>
        askFor(ActorManager.devices, AddDeviceToSystem(docNumber, stock, units, count, label, forLabel))
      },
      (get & path("removeDeviceFromSystem") & parameter("docNumber") & parameter("stock") & parameter("units") & parameter("count") & parameter("label") & parameter("forLabel")) { (docNumber, stock, units, count, label, forLabel) =>
        askFor(ActorManager.devices, RemoveDeviceFromSystem(docNumber, stock, units, count, label, forLabel))
      },

      //ACCOMMODATIONS
      (get & path("accommodations") & parameter("docNumber")) { (docNumber) =>
        askFor(ActorManager.accommodations, GetAccommodations(docNumber))
      },
      (get & path("accommodationsEspFiles") & parameter("docNumber") & parameter("revision") & parameter("lang")) { (docNumber, revision, lang) =>
        askFor(ActorManager.accommodations, GetAccommodationsESP(docNumber, revision, lang))
      },
      (get & path("addGroupToSystem") & parameter("docNumber") & parameter("stock") & parameter("userId")) { (docNumber, stock, userId) =>
        askFor(ActorManager.accommodations, AddAccommodationGroup(docNumber, stock, userId))
      },
      (get & path("setAccommodationLabel") & parameter("docNumber") & parameter("userId") & parameter("oid")) { (docNumber, userId, oid) =>
        askFor(ActorManager.accommodations, SetAccommodationLabel(docNumber, userId, oid))
      },

      (get & path("qrCode") & parameter("url")) { (url) =>
        askFor(ActorManager.spec, GenerateQRCode(url))
      },
      (get & path("eqFoundations") & parameter("project")) { (project) =>
        askFor(ActorManager.spec, GetEqFoundations(project))
      },

      (get & path("eqFoundationsUpdateStatus") & parameter("project") & parameter("id") & parameter("user")) { (project, id, user) =>
        askFor(ActorManager.spec, UpdateStatusEqFoundations(project, id, user))
      },
    )
  }

  def askFor(actor: ActorRef, command: Any, long: Boolean = false): Route = {
    try {
      Await.result(actor ? command, timeout.duration) match {
        case response: JsValue => complete(HttpEntity(response.toString()))
        case response: Array[Byte] => complete(HttpEntity(response))
        case response: String => complete(HttpEntity(response))
        case response: UniversalEntity => complete(response)
        case response: File => getFromFile(response)
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
