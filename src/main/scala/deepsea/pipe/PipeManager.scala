package deepsea.pipe

import akka.actor.{Actor, ActorSystem}
import akka.http.scaladsl.{Http, HttpExt}
import akka.pattern.ask
import akka.util.Timeout
import deepsea.actors.ActorManager
import deepsea.database.DatabaseManager
import deepsea.database.DatabaseManager.{GetConnection, GetMongoCacheConnection, GetMongoConnection, GetOracleConnection}
import deepsea.files.FileManager.GenerateUrl
import deepsea.pipe.PipeManager.{GetPipeESP, GetPipeSegs, GetPipeSegsBilling, GetPipeSegsByDocNumber, GetSpoolLocks, GetSpoolModel, GetSystems, GetZones, Material, PipeSeg, PipeSegActual, PipeSegBilling, ProjectName, SetSpoolLock, SpoolLock, SystemDef, UpdatePipeComp, UpdatePipeJoints}
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.model.Filters.{all, and, equal, in, notEqual}
import org.mongodb.scala.{Document, MongoCollection, bson}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import local.common.Codecs
import org.mongodb.scala.model.Filters
import io.circe.parser.decode
import local.pdf.en.pipe.SpoolsReportEN.{genSpoolsListEnPDF, genSpoolsListEnPDFAll}

import java.util.concurrent.TimeUnit
import java.util.{Date, UUID}
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.concurrent.duration.{Duration, DurationInt, SECONDS}


object PipeManager{

  case class PipeSeg(project: String, zone: String, system: String, line: String, pls: Int, elem: Int, typeCode: String, typeDesc: String, classAlpha: String, compType: String, compUserId: String, smat: String, sqInSystem: Int, isPieceId: Int, spPieceId: Int, isom: String, spool: String, var length: Double, radius: Double, angle: Double, weight: Double, stock: String, fcon3: String, insul: String, var material: Material = Material(), var systemDescr: String = "")
  case class PipeSegBilling(zone: String, system: String, typeCode: String, typeDesc: String, classAlpha: String, compType: String, compUserId: String, smat: String, length: Double, weight: Double, stock: String, insul: String, material: Material = Material(), systemDescr: String = "", count: Int = 1)
  case class PipeSegActual(name: String, date: Long)
  case class Material(
                       name: String = "",
                       description: String = "",
                       category: String = "",
                       code: String = "",
                       units: String = "",
                       singleWeight: Double = 0,
                       project: String = "",
                       document: String = "",
                       provider: String = "",
                       note: String = "",
                       comment: String = "",
                       manufacturer: String = "",
                       coefficient: Double = 1,
                       id: String = UUID.randomUUID().toString,
                       translations: List[MaterialTranslation] = List.empty[MaterialTranslation],
                       itt: Int = 0,
                       approved: Int = 0){
    def name(lang: String = "en"): String ={
      translations.find(_.lang == lang) match {
        case Some(translation) => translation.name
        case _ => name
      }
    }
    def description(lang: String = "en"): String ={
      translations.find(_.lang == lang) match {
        case Some(translation) => translation.description
        case _ => description
      }
    }
  }
  case class Units(code: String, name: String, thumb: String, translations: List[UnitTranslation] = List.empty[UnitTranslation]){
    def name(lang: String = "en"): String ={
      translations.find(_.lang == lang) match {
        case Some(translation) => translation.name
        case _ => name
      }
    }
    def thumb(lang: String = "en"): String ={
      translations.find(_.lang == lang) match {
        case Some(translation) => translation.thumb
        case _ => thumb
      }
    }
  }
  case class MaterialTranslation(lang: String, name: String, description: String)
  case class UnitTranslation(lang: String, name: String, thumb: String)
  case class ProjectName(id: String, rkd: String, pdsp: String, foran: String, cloud: String, cloudRkd: String)
  case class SystemDef(project: String, name: String, descr: String)
  case class SpoolLock(issueId: Int, docNumber: String, spool: String, var lock: Int, user: String, var date: Long)

  case class UpdatePipeComp()
  case class UpdatePipeJoints()
  case class GetPipeSegs(project: String, system: String = "", sqInSystem: String = "")
  case class GetPipeSegsBilling(project: String)
  case class GetPipeSegsSpools(project: String)
  case class GetPipeSegsByDocNumber(docNumber: String, json: Boolean = true)
  case class GetSystems(project: String)
  case class GetZones(project: String)
  case class GetSpoolLocks(docNumber: String)
  case class SetSpoolLock(jsValue: String)
  case class GetPipeESP(docNumber: String, revision: String, bySpool: String, lang: String)
  case class GetSpoolModel(docNumber: String, spool: String, isom: String)


}
class PipeManager extends Actor with Codecs with PipeHelper {


  implicit val timeout: Timeout = Timeout(60, TimeUnit.SECONDS)

  override def preStart(): Unit ={
    //self ! GetSpoolModel("210101-545-0001", "032", "0")
  }

  override def receive: Receive = {
    case GetSystems(project) => sender() ! getSystems(project).asJson.noSpaces
    case GetZones(project) => sender() ! getZones(project).asJson.noSpaces
    case GetPipeSegs(project, system, sqInSystem) => sender() ! getPipeSegs(project, system, sqInSystem.toIntOption.getOrElse(-1)).asJson.noSpaces
    case GetPipeSegsBilling(project) => sender() ! getPipeSegsBilling(project).asJson.noSpaces
    case GetPipeSegsByDocNumber(docNumber, json) =>
      val projectSystem = getSystemAndProjectFromDocNumber(docNumber)
      val pipeSegs = getPipeSegs(projectSystem._1, projectSystem._2)
      sender() ! (if (json) pipeSegs.asJson.noSpaces else pipeSegs)
    case SetSpoolLock(jsValue) =>
      setSpoolLock(jsValue)
      sender() ! "success".asJson.noSpaces
    case GetSpoolLocks(docNumber) =>
      sender() ! getSpoolLocks(docNumber).asJson.noSpaces
    case GetPipeESP(docNumber: String, revision: String, bySpool: String, lang: String) =>
      val projectSystem = getSystemAndProjectFromDocNumber(docNumber)
      val pipeSegs = getPipeSegs(projectSystem._1, projectSystem._2)
      val systemDefs = getSystemDefs(projectSystem._1)
      val systemDescr = systemDefs.find(_.name == projectSystem._2) match {
        case Some(value) => value.descr.replace(docNumber, "").trim
        case _ => "NO DESCR"
      }
      val rev = if (revision == "NO REV")  "" else revision
      val file = if (bySpool == "1") genSpoolsListEnPDF(docNumber, systemDescr, rev, pipeSegs, lang) else genSpoolsListEnPDFAll(docNumber, systemDescr, revision, pipeSegs, lang)
      Await.result(ActorManager.files ? GenerateUrl(file), timeout.duration) match {
        case url: String => sender() ! url.asJson.noSpaces
        case _ => sender() ! "error".asJson.noSpaces
      }
    case GetSpoolModel(docNumber, spool, isom) =>
      sender() ! getSpoolModel(docNumber, spool, isom.toIntOption.getOrElse(0))
    case _ => None

  }

}
