package deepsea.esp

import akka.actor.Actor
import deepsea.database.DBManager
import deepsea.esp.EspManager.{AddMaterialPurchase, CreateEsp, EspObject, GetEsp, GetGlobalEsp, GetHullEsp, GetMaterialPurchases, GlobalEsp, HullEspObject, InitIssues, Issue, MaterialPurchase, PipeEspObject}
import deepsea.pipe.PipeHelper
import deepsea.pipe.PipeManager.{Material, PipeSeg, ProjectName}
import io.circe.generic.JsonCodec
import io.circe.{Decoder, Encoder}
import io.circe.syntax.EncoderOps
import local.common.Codecs
import local.pdf.ru.common.ReportCommon.Item11Columns
import io.circe._
import io.circe.generic.JsonCodec
import io.circe.parser.decode
import io.circe.syntax._
import local.ele.CommonEle.EleComplectParts
import local.hull.PartManager.{ForanPartsByDrawingNum, PrdPart}
import org.mongodb.scala.MongoCollection

import java.util.{Date, UUID}
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

object EspManager{

  val espObjectsCollectionName = "esp-objects"
  val espKinds: List[String] = List("hull", "pipe")

  case class HullEspObject(override val id: String,
                           override val foranProject: String,
                           override val docNumber: String,
                           override val rev: String,
                           override val date: Long,
                           override val user: String,
                           override val kind: String,
                           override val taskId: Int,
                           elements: List[PrdPart]) extends EspObject(id, foranProject, docNumber, rev, date, user, kind, taskId)
  case class PipeEspObject(override val id: String,
                           override val foranProject: String,
                           override val docNumber: String,
                           override val rev: String,
                           override val date: Long,
                           override val user: String,
                           override val kind: String,
                           override val taskId: Int,
                           elements: List[PipeSeg]) extends EspObject(id, foranProject, docNumber, rev, date, user, kind, taskId)

  case class EspHistoryObject(override val id: String,
                              override val foranProject: String,
                              override val docNumber: String,
                              override val rev: String,
                              override val date: Long,
                              override val user: String,
                              override val kind: String,
                              override val taskId: Int) extends EspObject(id, foranProject, docNumber, rev, date, user, kind, taskId)
  abstract class EspObject(val id: String, val foranProject: String, val docNumber: String, val rev: String, val date: Long, val user: String, val kind: String, val taskId: Int)
  trait EspElement


  case class CreateEsp(foranProject: String, docNumber: String, rev: String, user: String, kind: String, taskId: String)
  case class GetHullEsp(foranProject: String, docNumber: String, rev: String = "")
  case class GetPipeEsp(foranProject: String, docNumber: String, rev: String = "")
  case class GetEsp(foranProject: String, kind: String, docNumber: String, rev: String = "")

  case class Issue(id: Int, project: String, issue_type: String, doc_number: String, revision: String, department: String)
  case class InitIssues()
  case class GetGlobalEsp(projects: String, kinds: String)
  case class MaterialPurchase(code: String, project: String, date: Long, user: String, qty: Double, contract: String)
  case class AddMaterialPurchase(materialPurchase: String)
  case class GetMaterialPurchases(project: String)
  case class MaterialSummary(material: Material, name: String, descr: String, unitsName: String, qty: Double, singleWeight: Double, totalWeight: Double, drawings: List[String])

  case class ExportPipeFittings()
  case class GlobalEsp(code: String, name: String, desc: String, units: String, unitsValue: String, qty: Double, weight: Double, weightTotal: Double, documents: List[DocumentWithMaterial], material: Material)
  case class DocumentWithMaterial(docNumber: String, rev: String, user: String, date: Long, units: String, unitsValue: String, qty: Double, weight: Double, totalWeight: Double, label: String)
}

class EspManager extends Actor with EspManagerHelper with Codecs with PipeHelper {

  override def preStart(): Unit = {
    val qwe = generateGlobalEsp(List("N002"))
    val q = qwe
//    generatePipeGlobalEsp(List("N002"))
//    self ! InitIssues()
//    val qw = getAllLatestEsp()
//    val jk = qw
//    self ! CreateEsp("N002", "200101-222-104", "C", "isaev", "hull")
  }
  override def receive: Receive = {
    case CreateEsp(foranProject, docNumber, rev, user, kind, taskId) =>
      val id = UUID.randomUUID().toString
      val date = new Date().getTime
      kind match {
        case "hull" =>
          addHullEsp(HullEspObject(id, foranProject, docNumber, rev, date, user, kind, taskId.toIntOption.getOrElse(0), elements = ForanPartsByDrawingNum(foranProject, docNumber)))
        case "pipe" =>
          val projectSystem = getSystemAndProjectFromDocNumber(docNumber)
          addPipeEsp(PipeEspObject(id, foranProject, docNumber, rev, date, user, kind, taskId.toIntOption.getOrElse(0), elements = getPipeSegs(projectSystem._1, projectSystem._2)))
        case _ => Option.empty[EspObject]
      }
      sender() ! "success".asJson.noSpaces
    case GetEsp(foranProject, kind, docNumber, rev) =>
      kind match {
        case "hull" =>
          sender() ! getHullLatestEsp(foranProject, kind, docNumber, rev).asJson.noSpaces
        case "pipe" =>
          sender() ! getPipeLatestEsp(foranProject, kind, docNumber, rev).asJson.noSpaces
        case _ =>
          sender() ! "error".asJson.noSpaces
      }
    case InitIssues() =>
      DBManager.GetMongoConnection() match {
        case Some(mongo) =>
          val issues = ListBuffer.empty[Issue]
          DBManager.GetPGConnection() match {
            case Some(c) =>
              val s = c.createStatement()
              val query = "select * from issue"
              val rs = s.executeQuery(query)
              while (rs.next()){
                issues += Issue(
                  rs.getInt("id"),
                  rs.getString("project"),
                  rs.getString("issue_type"),
                  rs.getString("doc_number"),
                  rs.getString("revision"),
                  rs.getString("department"),
                )
              }
              s.close()
              c.close()
          }
          val date = new Date().getTime
          val id = UUID.randomUUID().toString
          val projectNamesCollection: MongoCollection[ProjectName] = mongo.getCollection("project-names")
          val projectNames = Await.result(projectNamesCollection.find().toFuture(), Duration(30, SECONDS)) match {
            case values: Seq[ProjectName] => values.toList
            case _ => List.empty[ProjectName]
          }
          issues.filter(x => x.issue_type == "RKD").groupBy(_.doc_number).foreach(issue => {
            projectNames.find(_.pdsp == issue._2.head.project) match {
              case Some(project) =>
                issue._2.head.department match {
                  case "Hull" =>
                    val parts = ForanPartsByDrawingNum(project.foran, issue._1)
                    if (parts.nonEmpty){
                      addHullEsp(HullEspObject(id, project.foran, issue._2.head.doc_number, issue._2.head.revision, date, "op", "hull", issue._2.head.id, elements = parts))
                    }
                  case "System" =>
                    val projectSystem = getSystemAndProjectFromDocNumber(issue._2.head.doc_number)
                    addPipeEsp(PipeEspObject(id, project.foran, issue._2.head.doc_number, issue._2.head.revision, date, "op", "pipe", issue._2.head.id, elements = getPipeSegs(projectSystem._1, projectSystem._2)))
                  case _ => None
                }
              case _ => None
            }
          })
        case _ => None
      }
    case GetGlobalEsp(projects, kinds) =>
      val hull = if (kinds.contains("hull")) generateHullGlobalEsp(projects.split(",").toList) else List.empty[GlobalEsp]
      val pipe = if (kinds.contains("pipe")) generatePipeGlobalEsp(projects.split(",").toList) else List.empty[GlobalEsp]
      val globalEsp = hull ++ pipe
      sender() ! globalEsp.asJson.noSpaces
    case AddMaterialPurchase(materialPurchaseValue) =>
      decode[MaterialPurchase](materialPurchaseValue) match {
        case Right(materialPurchase) =>
          addMaterialPurchase(materialPurchase)
        case Left(value) =>
      }
      sender() ! "success".asJson.noSpaces
    case GetMaterialPurchases(project) =>
      sender() ! getMaterialPurchases(project).asJson.noSpaces
    case _ => None
  }
}
