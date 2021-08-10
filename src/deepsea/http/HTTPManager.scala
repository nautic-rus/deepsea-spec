package deepsea.http

import akka.Done
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.{Actor, ActorRef}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Multipart.BodyPart
import akka.http.scaladsl.model.{HttpEntity, Multipart}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import akka.pattern.ask
import akka.stream.scaladsl.{FileIO, Sink}
import akka.util.Timeout
import deepsea.App
import deepsea.actors.ActorManager
import deepsea.actors.ActorStartupManager.HTTPManagerStarted
import deepsea.auth.AuthManager.{GetUsers, Login}
import deepsea.camunda.CamundaManager.{DeployProcess, GetDeploymentResource, GetUploadedDeployments}
import deepsea.files.FileManager.CreateFile
import deepsea.issues.IssueManager.{GetIssueDetails, GetIssueProjects, GetIssueTypes, GetIssues, RemoveIssue, SetIssueMessage, SetIssueStatus, StartIssue}
import org.apache.log4j.{LogManager, Logger}
import play.api.libs.json.{JsValue, Json}

import java.io.{File, FileInputStream, InputStream}
import java.util.concurrent.TimeUnit
import scala.concurrent.{Await, ExecutionContextExecutor, Future}

object HTTPManager{
  case class Response(value: String)
}
class HTTPManager extends Actor{
  implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "http")
  implicit val executionContext: ExecutionContextExecutor = system.executionContext
  implicit val timeout: Timeout = Timeout(30, TimeUnit.SECONDS)
  val logger: Logger = LogManager.getLogger("HttpManager")
  var server:  Future[Http.ServerBinding] = _
  val routes: Route = {
    concat(
      //AUTHORIZATION COMMANDS
      (get & path("login") & parameter("login", "password")){(login, password) =>
        askFor(ActorManager.auth, Login(Option.empty[String], login, password))
      },
      (get & path("login") & parameter("token")){ token =>
        askFor(ActorManager.auth, Login(Option(token)))
      },
      (get & path("users")){
        askFor(ActorManager.auth, GetUsers())
      },
      //ISSUE MANAGER COMMANDS
      (get & path("issueProjects")){
        askFor(ActorManager.issue, GetIssueProjects())
      },
      (get & path("issueTypes")){
        askFor(ActorManager.issue, GetIssueTypes())
      },
      (get & path("issueTypes")){
        askFor(ActorManager.issue, GetIssueTypes())
      },
      (get & path("issues") & parameter("user")){ user =>
        askFor(ActorManager.issue, GetIssues(user))
      },
      (post & path("startIssue") & parameter("user") & entity(as[String])){ (user, issue) =>
        askFor(ActorManager.issue, StartIssue(user, issue))
      },
      (get & path("removeIssue") & parameter("id")){ id =>
        askFor(ActorManager.issue, RemoveIssue(id))
      },
      (get & path("issueDetails") & parameter("id") & parameter("user")){ (id, user) =>
        askFor(ActorManager.issue, GetIssueDetails(id, user))
      },
      (get & path("setIssueStatus") & parameter("id") & parameter("user") & parameter("status")){ (id, user, status) =>
        askFor(ActorManager.issue, SetIssueStatus(id, user, status))
      },
      (get & path("getUploadedDeployments")){
        askFor(ActorManager.camunda, GetUploadedDeployments())
      },
      (get & path("getDeploymentResource") & parameter("id")){ id =>
        askFor(ActorManager.camunda, GetDeploymentResource(id))
      },
      (post & path("setIssueMessage") & parameter("id") & entity(as[String])){ (id, message) =>
        askFor(ActorManager.issue, SetIssueMessage(id, message))
      },
      (post & path("deployProcess") & entity(as[Multipart.FormData])){ formData =>
        var fileName = ""
        var fileStream: InputStream = null
        val done: Future[Done] = formData.parts.mapAsync(1) {
          case b: BodyPart if b.name == "file" =>
            val file = File.createTempFile("upload", "tmp")
            b.entity.dataBytes.runWith(FileIO.toPath(file.toPath))
            fileName = b.filename.get
            fileStream = new FileInputStream(file)
            Future.successful(Done)
          case _ => Future.successful(Done)
        }.runWith(Sink.ignore)
        onSuccess(done) { _ =>
          askFor(ActorManager.camunda, DeployProcess(fileName, fileStream))
        }
      },
      //FILE MANAGER COMMANDS
      (post & path("createFileUrl") & entity(as[Multipart.FormData])){ formData =>
        var fileName = ""
        var fileStream: InputStream = null
        val done: Future[Done] = formData.parts.mapAsync(1) {
          case b: BodyPart if b.name == "file" =>
            val file = File.createTempFile("upload", "tmp")
            b.entity.dataBytes.runWith(FileIO.toPath(file.toPath))
            fileName = b.filename.get
            fileStream = new FileInputStream(file)
            file.delete()
            Future.successful(Done)
          case _ => Future.successful(Done)
        }.runWith(Sink.ignore)
        onSuccess(done) { _ =>
          askFor(ActorManager.files, CreateFile(fileName, fileStream), long = true)
        }
      },

    )
  }

  def askFor(actor: ActorRef, command: Any, long: Boolean = false): StandardRoute ={
    try{
      Await.result(actor ? command, timeout.duration) match {
        case response: JsValue => complete(HttpEntity(response.toString()))
        case response: Array[Byte] => complete(HttpEntity(response))
        case response: String => complete(HttpEntity(Json.toJson(response).toString()))
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
