package deepsea.pipe

import akka.actor.Actor
import akka.pattern.ask
import akka.util.Timeout
import deepsea.actors.ActorManager
import deepsea.esp.EspManager.{EspElement, GetEsp}
import deepsea.files.FileManager.GenerateUrl
import deepsea.pipe.PipeManager._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import io.circe.syntax.EncoderOps
import local.common.Codecs
import local.pdf.en.pipe.SpoolsReportEN.{genSpoolsListEnPDF, genSpoolsListEnPDFAll}
import slick.lifted.Tag

import java.util.UUID
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.language.postfixOps
import slick.jdbc.PostgresProfile.api._
import slick.lifted.{ProvenShape, TableQuery}


object PipeManager{

  case class PipeSeg(project: String, zone: String, system: String, line: String,
                     pls: Int, elem: Int, typeCode: String, typeDesc: String, classAlpha: String, compType: String, compUserId: String, smat: String,
                     sqInSystem: Int, isPieceId: Int, var spPieceId: Int, isom: String, spool: String, var length: Double, radius: Double, angle: Double,
                     weight: Double, stock: String, fcon3: String, insul: String, classDescription: String, var material: Material = Material(), var systemDescr: String = "") extends EspElement {
  }
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
        case Some(translation) => if (translation.name != "") translation.name else name
        case _ => name
      }
    }
    def description(lang: String = "en"): String ={
      translations.find(_.lang == lang) match {
        case Some(translation) => if (translation.description != "") translation.description else description
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
  case class MaterialTranslation(lang: String, var name: String, description: String)
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
  case class PipeSup(code: String, userId: String)
  case class GetHvacSegs(docNumber: String)


  case class SpecMaterial(code: String, name: String, descr: String, units: String, weight: Double, supplier: String, statem_id: Int, dir_id: Int, user_id: Int, label: String, last_upd: Long, note: String, manufacturer: String, coef: Double, id: Int, removed: Int)
  class SpecMaterialTable(tag: Tag) extends Table[SpecMaterial](tag, "materials") {
    val code = column[String]("stock_code")
    val name = column[String]("name")
    val descr = column[String]("description")
    val units = column[String]("unit")
    val weight = column[Double]("weight")
    val supplier = column[String]("supplier")
    val statem_id = column[Int]("statement_id")
    val dir_id = column[Int]("directory_id")
    val user_id = column[Int]("user_id")
    val label = column[String]("default_label")
    val last_upd = column[Long]("last_update")
    val note = column[String]("note")
    val manufacturer = column[String]("manufacturer")
    val coef = column[Double]("coef")
    val id = column[Int]("id", O.AutoInc, O.PrimaryKey)
    val removed = column[Int]("removed")
    override def * = (code, name, descr, units, weight, supplier, statem_id, dir_id, user_id, label, last_upd, note, manufacturer, coef, id, removed) <> ((SpecMaterial.apply _).tupled, SpecMaterial.unapply)
  }
  implicit val SpecMaterialDecoder: Decoder[SpecMaterial] = deriveDecoder[SpecMaterial]
  implicit val SpecMaterialEncoder: Encoder[SpecMaterial] = deriveEncoder[SpecMaterial]

  case class MaterialStatement(id: Int, name: String, project_id: Int, code: String, parent_id: Int, doc_number: String)
  class MaterialStatementTable(tag: Tag) extends Table[MaterialStatement](tag, "materials_statements") {
    val id = column[Int]("id", O.AutoInc)
    val name = column[String]("name")
    val project_id = column[Int]("project_id")
    val code = column[String]("code")
    val parent_id = column[Int]("parent_id")
    val doc_number = column[String]("doc_number")
    override def * = (id, name, project_id, code, parent_id, doc_number) <> ((MaterialStatement.apply _).tupled, MaterialStatement.unapply)
  }
  implicit val MaterialStatementDecoder: Decoder[MaterialStatement] = deriveDecoder[MaterialStatement]
  implicit val MaterialStatementEncoder: Encoder[MaterialStatement] = deriveEncoder[MaterialStatement]

  case class MaterialDirectory(id: Int, name: String, parent_id: Int, user_id: Int, date: Long, old_code: String, project_id: Int, removed: Int)
  class MaterialDirectoryTable(tag: Tag) extends Table[MaterialDirectory](tag, "materials_directory") {
    val id = column[Int]("id", O.AutoInc, O.PrimaryKey)
    val name = column[String]("name")
    val parent_id = column[Int]("parent_id")
    val user_id = column[Int]("user_id")
    val date = column[Long]("date")
    val old_code = column[String]("old_code")
    val project_id = column[Int]("project_id")
    val removed = column[Int]("removed")
    override def * = (id, name, parent_id, user_id, date, old_code, project_id, removed) <> ((MaterialDirectory.apply _).tupled, MaterialDirectory.unapply)
  }
  implicit val MaterialDirectoryDecoder: Decoder[MaterialDirectory] = deriveDecoder[MaterialDirectory]
  implicit val MaterialDirectoryEncoder: Encoder[MaterialDirectory] = deriveEncoder[MaterialDirectory]

  case class Pls(typeCode: Int, zone: Int, system: Int, line: String, pls: Int, elem: Int){
    def equals(that: Pls): Boolean = {
      typeCode == that.typeCode &&
      zone == that.zone &&
      system == that.system &&
      line == that.line &&
      pls == that.pls &&
      (elem == that.elem || elem == 0 || that.elem == 0)
    }
  }
  case class PlsElem(pls: Pls, weight: Double, isomId: Int, spoolId: Int, isPieceId: Int, spPieceId: Int, cType: String, idsq: Int, cmp_oid: Int, cmp_stock: String, zone: String, spool: String, isom: String, classDescr: String, cmpName: String)
  case class PipeLineSegment(pls: Pls, bdatri: String, oid: Int)
  case class PlsParam(pls: Pls, paramObj: Int, paramSq: Int, value: Double)
  case class MaterialQuality(code: String, descr: String, weight: Double, thickness: Double)
  case class ElecEquip(comp: Int, userId: String, zone: String, stock: String)
}
class PipeManager extends Actor with Codecs with PipeHelper {


  implicit val timeout: Timeout = Timeout(10, TimeUnit.SECONDS)

  override def preStart(): Unit ={
    //self ! GetPipeSegs("N002", "710-005")
    //self ! GetPipeESP("200101-743-001", "1", "", "ru")
    //self ! GetPipeSegsByDocNumber("200101-743-001")
    //self ! GetSpoolModel("210101-545-0001", "032", "0")
  }

  override def receive: Receive = {
    case GetSystems(project) => sender() ! getSystems(project).asJson.noSpaces
    case GetZones(project) => sender() ! getZones(project).asJson.noSpaces
    case GetPipeSegs(project, system, sqInSystem) =>
//      val jk = getPipeSegs(project, system, sqInSystem.toIntOption.getOrElse(-1))
//      genSpoolsListEnPDFAll("200101-574-014", "FUEL SYSTEM", "0", jk,"en")
      sender() ! getPipeSegs(project, system, sqInSystem.toIntOption.getOrElse(-1)).asJson.noSpaces
    case GetPipeSegsBilling(project) => sender() ! getPipeSegsBilling(project).asJson.noSpaces
    case GetPipeSegsByDocNumber(docNumber, json) =>
      val projectSystem = getSystemAndProjectFromDocNumber(docNumber)
      Await.result(ActorManager.esp ? GetEsp(projectSystem._1, "pipe", docNumber), timeout.duration) match {
        case res: String => sender() ! res
        case _ => sender() ! "error".asJson.noSpaces
      }
      //val pipeSegs = getPipeSegs(projectSystem._1, projectSystem._2)
      //sender() ! (if (json) pipeSegs.asJson.noSpaces else pipeSegs)
    case SetSpoolLock(jsValue) =>
      setSpoolLock(jsValue)
      sender() ! "success".asJson.noSpaces
    case GetSpoolLocks(docNumber) =>
      sender() ! getSpoolLocks(docNumber).asJson.noSpaces
    case GetPipeESP(docNumber: String, revision: String, bySpool: String, lang: String) =>
      val projectSystem = getSystemAndProjectFromDocNumber(docNumber)
      val pipeSegs = getPipeSegs(projectSystem._1, projectSystem._2).filter(_.spool != "")
      val systemDefs = getSystemDefs(projectSystem._1)
      val systemDescr = systemDefs.find(_.name == projectSystem._2) match {
        case Some(value) => value.descr.replace(docNumber, "").trim
        case _ => "NO DESCR"
      }
      val rev = if (revision == "NO REV") "" else revision
      val materials = getMaterials.filter(_.project == docNumber.split("-").headOption.getOrElse(""))
      val file = if (bySpool == "1") genSpoolsListEnPDF(docNumber, systemDescr, rev, pipeSegs, lang, materials = materials) else genSpoolsListEnPDFAll(docNumber, systemDescr, revision, pipeSegs, lang, materials)
      Await.result(ActorManager.files ? GenerateUrl(file), timeout.duration) match {
        case url: String => sender() ! url.asJson.noSpaces
        case _ => sender() ! "error".asJson.noSpaces
      }
    case GetSpoolModel(docNumber, spool, isom) =>
      sender() ! getSpoolModel(docNumber, spool, isom.toIntOption.getOrElse(0))
    case GetHvacSegs(docNumber) =>
      getHvacSegs(docNumber)
    case _ => None

  }

}
