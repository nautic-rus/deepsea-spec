package deepsea.hull

import akka.actor.Actor
import akka.pattern.ask
import akka.util.Timeout
import com.mongodb.BasicDBObject
import deepsea.App
import deepsea.actors.ActorManager
import deepsea.database.DBManager
import deepsea.esp.EspManager.{GetEsp, GetHullEsp}
import deepsea.files.FileManager.GenerateUrl
import deepsea.hull.HullManager._
import deepsea.hull.classes.HullPart
import deepsea.materials.MaterialsHelper
import deepsea.pipe.PipeManager.{Material, PipeSeg, PipeSegActual, ProjectName}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import local.sql.MongoDB
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase}
import org.mongodb.scala.bson.codecs.Macros._
import org.mongodb.scala.model.Filters.{and, equal}

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, FiniteDuration, SECONDS}
import io.circe._
import io.circe.parser._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.generic.semiauto._
import local.hull.BStree.BsTreeItem
import local.hull.PartManager.{ForanPartsByDrawingNum, PrdPart, genForanPartLabelByDrawingNumAndPartNameJSON, genForanPartsByDrawingNum, genForanPartsByDrawingNumJSON}
import local.hull.bill.BillManager.{genAnalyticPlateDataJson, genAnalyticProfileDataJson}
import local.pdf.en.common.ReportCommonEN.{DocNameEN, Item11ColumnsEN}
import local.pdf.en.prd.PrdPartsReportEN.genHullPartListEnPDF
import local.sql.MongoDB.mongoClient
import org.apache.poi.xssf.usermodel.XSSFWorkbook

import java.io.{File, FileOutputStream}
import java.nio.file.Files
import java.sql.{Connection, ResultSet}
import java.util
import java.util.concurrent.TimeUnit
import java.util.zip.{ZipEntry, ZipOutputStream}
import java.util.{Date, UUID}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import local.common.Codecs

object HullManager {

  case class GetHullPartsExcel(project: String)

  case class GetHullPart(project: String, docNumber: String, partCode: String)

  case class GetHullPartsByDocNumber(project: String, docNumber: String)

//  case class GetHullEsp(docNumber: String, revision: String = "")
//
//  case class SetHullEsp(project: String, docNumber: String, user: String, revision: String)
//
  case class GetHullEspFiles(project: String, docNumber: String, docName: String, revision: String)

  case class HullPartPlateDef(PART_OID: Int, MATERIAL: String, MATERIAL_OID: Int, THICK: Double, STOCK_CODE: String)

  case class HullPartProfileDef(KSE: Int, SECTION: Int, WEB_HEIGHT: Double, WEB_THICKNESS: Double, FLANGE_HEIGHT: Double, FLANGE_THICKNESS: Double, MATERIAL: String, MATERIAL_OID: Int, STOCK_CODE: String)

  case class HullEsp(project: String, department: String, content: List[PrdPart], var docNumber: String, var user: String, var revision: String, var date: Long, var version: Int)

  case class GetHullPlatesForMaterial(project: String, material: String, thickness: String)
  case class GetHullProfilesForMaterial(project: String, material: String, kse: String)

  case class GetBsDesignNodes(project: String)
  case class GetHullSystems(project: String)
  case class BsDesignNode(OID: Int, TYPE: String, NAME: String, DESCRIPTION: String, PARENT_NODE: Int, ATOM_TYPE: Int, BLOCK_OID: Int, WEIGHT: Double, X_COG: Double, Y_COG: Double, Z_COG: Double, ATOM_NAME: String, DNA: String, DATE: Long, BLOCK: String, ZONE: String, USERID: String, ELEC: Int, POS: Int, Xmin: Double, Ymin: Double, Zmin: Double, Xmax: Double, Ymax: Double, Zmax: Double)

  implicit val HullEspDecoder: Decoder[HullEsp] = deriveDecoder[HullEsp]
  implicit val HullEspEncoder: Encoder[HullEsp] = deriveEncoder[HullEsp]

  case class RemoveParts(project: String, block: String, parts: String, user: String)

  case class PlatePart(code: String, block: String, name: String, description: String, weight: Double, thickness: Double, material: String, struct: String)
  case class ProfilePart(name: String, description: String, kse: Int, section: String, material: String, w_h: Double, w_t: Double, f_h: Double, f_t: Double, block: String, length: Double, area: Double, stock: String, density: Double, weight: Double, struct: String)
  case class HullSystem(code: String, name: String)
  case class AddIssueMaterial(pos: String, units: String, weight: String, count: String, stock: String, userId: String, docNumber: String, issueId: String, addText: String, department: String, zone: String)
  case class DeleteIssueMaterial(pos: String, docNumber: String, id: String, department: String)

  case class IssueMaterial(id: Int, pos: String, units: String, weight: Double, count: Double, stock: String, userId: Int, dateIns: Long, docNumber: String, issueId: Int, addText: String, dep: String, zone: String){
    def toHullPart(materials: List[Material], lang: String = "ru"): PrdPart = {
      materials.find(_.code == stock) match {
        case Some(material) =>
          if (material.code == "NR00000000021603"){
            val q = 0
          }
          val mWeight = units match {
            case "796" => material.singleWeight
            case "006" => material.singleWeight
            case "055" => material.singleWeight
            case _ => weight
          }
          PrdPart(id, count, pos, "", 0, "", material.name(lang), 0, material.description(lang), "MANUAL", "", 0, 0, 0, mWeight, mWeight * count, addText, 0, 0, 0, 0, 0, 0, 0, "", stock)
        case _ =>
          PrdPart(id, count, pos, "", 0, "", "MATERIAL NOT FOUND", 0, "MATERIAL NOT FOUND", "MANUAL", "", 0, 0, 0, weight, weight * count, "", 0, 0, 0, 0, 0, 0, 0, "", stock)
      }
    }
  }

}

class HullManager extends Actor with Codecs with HullHelper with MaterialsHelper {
  implicit val timeout: Timeout = Timeout(300, TimeUnit.SECONDS)

  private val espCollectionName = "hullEsp"

  override def receive: Receive = {
    //TOOLS
    case GetHullPartsExcel(project) =>
      val parts = getHullParts(project)
      val path = Files.createTempDirectory("foranParts")

      var wb = new XSSFWorkbook()
      var sheet = wb.createSheet("Plates")
      sheet.createRow(sheet.getLastRowNum + 1)
      sheet.getRow(sheet.getLastRowNum).createCell(0).setCellValue("MATERIAL")
      sheet.getRow(sheet.getLastRowNum).createCell(1).setCellValue("THICK")
      sheet.getRow(sheet.getLastRowNum).createCell(2).setCellValue("WEIGHT")
      parts.filter(x => x.KSE == 0).groupBy(x => (x.MATERIAL, x.THICK)).foreach(plateGroup => {
        val sum = plateGroup._2.map(x => x.WEIGHT).sum
        sheet.createRow(sheet.getLastRowNum + 1)
        sheet.getRow(sheet.getLastRowNum).createCell(0).setCellValue(plateGroup._1._1)
        sheet.getRow(sheet.getLastRowNum).createCell(1).setCellValue(plateGroup._1._2 * 1000)
        sheet.getRow(sheet.getLastRowNum).createCell(2).setCellValue(sum)
      })
      sheet = wb.createSheet("Profiles")
      sheet.createRow(sheet.getLastRowNum + 1)
      sheet.getRow(sheet.getLastRowNum).createCell(0).setCellValue("MATERIAL")
      sheet.getRow(sheet.getLastRowNum).createCell(1).setCellValue("SECTION")
      sheet.getRow(sheet.getLastRowNum).createCell(2).setCellValue("WEIGHT")
      parts.filter(x => x.KSE != 0).groupBy(x => (x.MATERIAL, x.SECTION)).foreach(profileGroup => {
        val sum = profileGroup._2.map(x => x.WEIGHT).sum
        sheet.createRow(sheet.getLastRowNum + 1)
        sheet.getRow(sheet.getLastRowNum).createCell(0).setCellValue(profileGroup._1._1)
        sheet.getRow(sheet.getLastRowNum).createCell(1).setCellValue(profileGroup._1._2)
        sheet.getRow(sheet.getLastRowNum).createCell(2).setCellValue(sum)
      })
      var file = new File(path.toString + File.separator + "total.xlsx")
      wb.write(new FileOutputStream(file))
      wb.close()

      parts.groupBy(x => x.BLOCKNAME).foreach(blockParts => {
        wb = new XSSFWorkbook()
        sheet = wb.createSheet("Plates")
        sheet.createRow(sheet.getLastRowNum + 1)
        sheet.getRow(sheet.getLastRowNum).createCell(0).setCellValue("MATERIAL")
        sheet.getRow(sheet.getLastRowNum).createCell(1).setCellValue("THICK")
        sheet.getRow(sheet.getLastRowNum).createCell(2).setCellValue("WEIGHT")
        blockParts._2.filter(x => x.KSE == 0).groupBy(x => (x.MATERIAL, x.THICK)).foreach(plateGroup => {
          val sum = plateGroup._2.map(x => x.WEIGHT).sum
          sheet.createRow(sheet.getLastRowNum + 1)
          sheet.getRow(sheet.getLastRowNum).createCell(0).setCellValue(plateGroup._1._1)
          sheet.getRow(sheet.getLastRowNum).createCell(1).setCellValue(plateGroup._1._2 * 1000)
          sheet.getRow(sheet.getLastRowNum).createCell(2).setCellValue(sum)
        })
        sheet = wb.createSheet("Profiles")
        sheet.createRow(sheet.getLastRowNum + 1)
        sheet.getRow(sheet.getLastRowNum).createCell(0).setCellValue("MATERIAL")
        sheet.getRow(sheet.getLastRowNum).createCell(1).setCellValue("SECTION")
        sheet.getRow(sheet.getLastRowNum).createCell(2).setCellValue("WEIGHT")
        blockParts._2.filter(x => x.KSE != 0).groupBy(x => (x.MATERIAL, x.SECTION)).foreach(profileGroup => {
          val sum = profileGroup._2.map(x => x.WEIGHT).sum
          sheet.createRow(sheet.getLastRowNum + 1)
          sheet.getRow(sheet.getLastRowNum).createCell(0).setCellValue(profileGroup._1._1)
          sheet.getRow(sheet.getLastRowNum).createCell(1).setCellValue(profileGroup._1._2)
          sheet.getRow(sheet.getLastRowNum).createCell(2).setCellValue(sum)
        })
        file = new File(path.toString + File.separator + blockParts._1 + ".xlsx")
        wb.write(new FileOutputStream(file))
        wb.close()
      })


      wb = new XSSFWorkbook()
      sheet = wb.createSheet("Plates")
      sheet.createRow(sheet.getLastRowNum + 1)
      sheet.getRow(sheet.getLastRowNum).createCell(0).setCellValue("MATERIAL")
      sheet.getRow(sheet.getLastRowNum).createCell(1).setCellValue("THICK")
      sheet.getRow(sheet.getLastRowNum).createCell(2).setCellValue("WEIGHT")
      parts.filter(x => x.KSE == 0).groupBy(x => (x.MATERIAL, x.THICK)).foreach(plateGroup => {
        val sum = plateGroup._2.map(x => x.WEIGHT).sum
        sheet.createRow(sheet.getLastRowNum + 1)
        sheet.getRow(sheet.getLastRowNum).createCell(0).setCellValue(plateGroup._1._1)
        sheet.getRow(sheet.getLastRowNum).createCell(1).setCellValue(plateGroup._1._2 * 1000)
        sheet.getRow(sheet.getLastRowNum).createCell(2).setCellValue(sum)
      })
      sheet = wb.createSheet("Profiles")
      sheet.createRow(sheet.getLastRowNum + 1)
      sheet.getRow(sheet.getLastRowNum).createCell(0).setCellValue("MATERIAL")
      sheet.getRow(sheet.getLastRowNum).createCell(1).setCellValue("SECTION")
      sheet.getRow(sheet.getLastRowNum).createCell(2).setCellValue("WEIGHT")
      parts.filter(x => x.KSE != 0).groupBy(x => (x.MATERIAL, x.SECTION)).foreach(profileGroup => {
        val sum = profileGroup._2.map(x => x.WEIGHT).sum
        sheet.createRow(sheet.getLastRowNum + 1)
        sheet.getRow(sheet.getLastRowNum).createCell(0).setCellValue(profileGroup._1._1)
        sheet.getRow(sheet.getLastRowNum).createCell(1).setCellValue(profileGroup._1._2)
        sheet.getRow(sheet.getLastRowNum).createCell(2).setCellValue(sum)
      })

      parts.groupBy(x => x.BLOCKNAME).foreach(blockParts => {
        sheet = wb.createSheet("Plates " + blockParts._1)
        sheet.createRow(sheet.getLastRowNum + 1)
        sheet.getRow(sheet.getLastRowNum).createCell(0).setCellValue("MATERIAL")
        sheet.getRow(sheet.getLastRowNum).createCell(1).setCellValue("THICK")
        sheet.getRow(sheet.getLastRowNum).createCell(2).setCellValue("WEIGHT")
        blockParts._2.filter(x => x.KSE == 0).groupBy(x => (x.MATERIAL, x.THICK)).foreach(plateGroup => {
          val sum = plateGroup._2.map(x => x.WEIGHT).sum
          sheet.createRow(sheet.getLastRowNum + 1)
          sheet.getRow(sheet.getLastRowNum).createCell(0).setCellValue(plateGroup._1._1)
          sheet.getRow(sheet.getLastRowNum).createCell(1).setCellValue(plateGroup._1._2 * 1000)
          sheet.getRow(sheet.getLastRowNum).createCell(2).setCellValue(sum)
        })
        sheet = wb.createSheet("Profiles " + blockParts._1)
        sheet.createRow(sheet.getLastRowNum + 1)
        sheet.getRow(sheet.getLastRowNum).createCell(0).setCellValue("MATERIAL")
        sheet.getRow(sheet.getLastRowNum).createCell(1).setCellValue("SECTION")
        sheet.getRow(sheet.getLastRowNum).createCell(2).setCellValue("WEIGHT")
        blockParts._2.filter(x => x.KSE != 0).groupBy(x => (x.MATERIAL, x.SECTION)).foreach(profileGroup => {
          val sum = profileGroup._2.map(x => x.WEIGHT).sum
          sheet.createRow(sheet.getLastRowNum + 1)
          sheet.getRow(sheet.getLastRowNum).createCell(0).setCellValue(profileGroup._1._1)
          sheet.getRow(sheet.getLastRowNum).createCell(1).setCellValue(profileGroup._1._2)
          sheet.getRow(sheet.getLastRowNum).createCell(2).setCellValue(sum)
        })
      })
      file = new File(path.toString + File.separator + "allInOne.xlsx")
      wb.write(new FileOutputStream(file))
      wb.close()

      val fileName = "bill-of-materials-" + new Date().getTime + ".zip"
      var pathId = UUID.randomUUID().toString.substring(0, 8)
      file = new File(App.Cloud.Directory + File.separator + pathId)
      while (file.exists()) {
        pathId = UUID.randomUUID().toString.substring(0, 8)
        file = new File(App.Cloud.Directory + File.separator + pathId)
      }
      file.mkdir()
      file = new File(App.Cloud.Directory + File.separator + pathId + File.separator + fileName)
      val zip = new ZipOutputStream(new FileOutputStream(file))
      Files.list(path).forEach(file => {
        zip.putNextEntry(new ZipEntry(file.getFileName.toString))
        zip.write(Files.readAllBytes(file))
      })
      zip.close()
      val fileUrl = App.Cloud.Url + "/" + pathId + "/" + fileName

      sender() ! fileUrl


    case GetHullPart(project, docNumber, partCode) => sender() ! genForanPartLabelByDrawingNumAndPartNameJSON(project, docNumber, partCode)

    //GET CURRENT TEMPLE PART LIST, GET AND SET ESP
    case GetHullPartsByDocNumber(project, docNumber) =>
      Await.result(ActorManager.esp ? GetEsp(project, "hull", docNumber), timeout.duration) match {
        case res: String => sender() ! res
        case _ => sender() ! "error".asJson.noSpaces
      }

      //sender() ! genForanPartsByDrawingNumJSON(project, docNumber)
//    case GetHullEsp(docNumber, revision) =>
//      val mongo = mongoClient()
//      val partLists = mongo.getDatabase(App.conf.getString("mongo.db")).getCollection(espCollectionName).withCodecRegistry(codecRegistry)
//      val partListsHistory = mongo.getDatabase(App.conf.getString("mongo.db")).getCollection(espCollectionName + "History").withCodecRegistry(codecRegistry)
//      val dbObject = new BasicDBObject()
//      dbObject.put("docNumber", docNumber)
//      if (revision != "") {
//        dbObject.put("revision", revision)
//      }
//      Await.result(partLists.find[HullEsp](dbObject).first().toFuture(), Duration(10, SECONDS)) match {
//        case existSP: HullEsp =>
//          sender() ! existSP.asJson.noSpaces
//        case _ =>
//          Await.result(partListsHistory.find[HullEsp](dbObject).first().toFuture(), Duration(10, SECONDS)) match {
//            case existSP: HullEsp =>
//              sender() ! existSP.asJson.noSpaces
//            case _ =>
//              sender() ! "not found"
//          }
//      }
//    case SetHullEsp(project, docNumber, user, revision) =>
//      val hullParts = ForanPartsByDrawingNum(project, docNumber).sortBy(s => s.PART_CODE)
//      val mongo = mongoClient()
//      val partLists = mongo.getDatabase(App.conf.getString("mongo.db")).getCollection(espCollectionName).withCodecRegistry(codecRegistry)
//      val partListsHistory = mongo.getDatabase(App.conf.getString("mongo.db")).getCollection(espCollectionName + "History").withCodecRegistry(codecRegistry)
//
//      var version = 0
//      Await.result(partLists.find[HullEsp](new BasicDBObject("docNumber", docNumber)).first().toFuture(), Duration(10, SECONDS)) match {
//        case existSP: HullEsp =>
//          version = existSP.version + 1
//          Await.result(partListsHistory.insertOne(Document.apply(existSP.asJson.noSpaces)).toFuture(), Duration(10, SECONDS))
//          Await.result(partLists.deleteOne(new BasicDBObject("docNumber", docNumber)).toFuture(), Duration(10, SECONDS))
//        case _ => None
//      }
//
//      val date = new Date().getTime
//      val hullPartList = HullEsp(project, "hull", hullParts, docNumber, user, revision, date, version)
//      Await.result(partLists.insertOne(Document.apply(hullPartList.asJson.noSpaces)).toFuture(), Duration(10, SECONDS))
//
//      sender() ! "success"
    case GetHullEspFiles(project, docNumber, docName, revision) =>
      val rev = if (revision == "NO REV")  "" else revision
      val file: String = Files.createTempDirectory("hullPdf").toAbsolutePath.toString + "/" + docNumber + "_rev" + rev + ".pdf"
      val rkdProject = docNumber.split("-").head
      val materials = getMaterials.filter(_.project == rkdProject)
      genHullPartListEnPDF(project, docNumber, docName, rev, file, getHullIssueMaterials(docNumber, materials))
      Await.result(ActorManager.files ? GenerateUrl(file), timeout.duration) match {
        case url: String => sender() ! url.asJson.noSpaces
        case _ => sender() ! "error".asJson.noSpaces
      }

    case GetHullPlatesForMaterial(project, material, thickness) =>
      sender() ! getPlates(project, material, thickness.toDoubleOption.getOrElse(0)).asJson.noSpaces

    case GetHullProfilesForMaterial(project, material, kse) =>
      sender() ! getProfiles(project, material, kse.toIntOption.getOrElse(0)).asJson.noSpaces

    case RemoveParts(project, block, parts, user) =>
      removeParts(project, block, parts, user)
      sender() ! "success"

    case GetHullSystems(project) =>
      sender() ! getSystems(project).asJson.noSpaces

    case GetBsDesignNodes(project) => sender() ! getBsDesignNodes(project).asJson.noSpaces

    case AddIssueMaterial(pos, units, weight, count, stock, userId, docNumber, issueId, addText, department, zone) =>
      addMaterial(pos, units, weight.toDoubleOption.getOrElse(0), count.toDoubleOption.getOrElse(0), stock, userId.toIntOption.getOrElse(0), docNumber, issueId.toIntOption.getOrElse(0), addText, department, zone)
      sender() ! "success".asJson.noSpaces
    case DeleteIssueMaterial(pos, docNumber, id, department) =>
      sender() ! deleteMaterial(pos, docNumber, id.toIntOption.getOrElse(0), department).asJson.noSpaces
  }


  def getHullPartsByDocNumber(project: String, docNumber: String): ListBuffer[HullPart] = {
    val parts = getHullParts(project, docNumber)
    val bsItems = ListBuffer.empty[BsTreeItem]
    parts.map(_.PARTOID).distinct.grouped(900).foreach(list => {
      bsItems ++= getBsTree(project, list)
    })
    parts.foreach(part => {
      bsItems.find(_.PP_OID == part.PARTOID) match {
        case Some(value) =>
          part.PARENT_NODE = value.PARENT_NODE
          part.ATOM_TYPE = value.ATOM_TYPE
          part.ORDINAL = value.ORDINAL
          part.TYPE_ATOM_TYPE = value.TYPE_ATOM_TYPE
          part.SYMBOL = value.SYMBOL
          part.LINE_NAME = value.LINE_NAME
          part.LINE_TYPE = value.LINE_TYPE
          part.LINE_TYPE_DESCRIPTION = value.LINE_TYPE_DESCRIPTION
          part.BS_ADN = value.BS_ADN
        case _ => None
      }
    })
    parts
  }
  def getBsTree(project: String, oids: ListBuffer[Int]): ListBuffer[BsTreeItem] = {
    DBManager.GetOracleConnection(project) match {
      case Some(c) =>
        val res = ListBuffer.empty[BsTreeItem]
        val query = Source.fromResource("queries/hullPartsFromBsTree.sql").mkString.replaceAll(":partOids", oids.mkString(","))
        val s = c.prepareStatement(query)
        val rs: ResultSet = s.executeQuery()
        while (rs.next()) {
          val PP_OID: Int = Option[Int](rs.getInt("PP_OID")).getOrElse(0)
          val EXPL_OID: Int = Option[Int](rs.getInt("EXPL_OID")).getOrElse(0)
          val BLOCK: String = Option[String](rs.getString("BLOCK")).getOrElse("")
          val CODE: String = Option[String](rs.getString("CODE")).getOrElse("")
          val PART_TYPE: Int = Option[Int](rs.getInt("PART_TYPE")).getOrElse(0)
          val DESCRIPTION: String = Option[String](rs.getString("DESCRIPTION")).getOrElse("")
          val PARENT_NODE: Int = Option[Int](rs.getInt("PARENT_NODE")).getOrElse(0)
          val ATOM_TYPE: Int = Option[Int](rs.getInt("ATOM_TYPE")).getOrElse(0)
          val ORDINAL: Int = Option[Int](rs.getInt("ORDINAL")).getOrElse(0)
          val TYPE_ATOM_TYPE: Int = Option[Int](rs.getInt("TYPE_ATOM_TYPE")).getOrElse(0)
          val SYMBOL: String = Option[String](rs.getString("SYMBOL")).getOrElse("")
          val WEIGHT: Double = Option[Double](rs.getDouble("WEIGHT")).getOrElse(0.0)
          val X_COG: Double = Option[Double](rs.getDouble("X_COG")).getOrElse(0.0)
          val Y_COG: Double = Option[Double](rs.getDouble("Y_COG")).getOrElse(0.0)
          val Z_COG: Double = Option[Double](rs.getDouble("Z_COG")).getOrElse(0.0)

          val PRF_LENGTH: Double = Option[Double](rs.getDouble("PRF_LENGTH")).getOrElse(0.0)
          val LINE_NAME: String = Option[String](rs.getString("LINE_NAME")).getOrElse("")
          val LINE_TYPE: String = Option[String](rs.getString("LINE_TYPE")).getOrElse("")
          val LINE_TYPE_DESCRIPTION: String = Option[String](rs.getString("LINE_TYPE_DESCRIPTION")).getOrElse("")
          val BS_ADN: String = Option[String](rs.getString("BS_ADN")).getOrElse("")
          val KSE: String = Option[String](rs.getString("KSE")).getOrElse("")
          val STOCKCODE: String = Option[String](rs.getString("STOCKCODE")).getOrElse("")
          res += BsTreeItem(
            PP_OID,
            EXPL_OID,
            BLOCK,
            CODE,
            PART_TYPE,
            DESCRIPTION,
            PARENT_NODE,
            ATOM_TYPE,
            ORDINAL,
            TYPE_ATOM_TYPE,
            SYMBOL,
            WEIGHT,
            X_COG,
            Y_COG,
            Z_COG,
            PRF_LENGTH,
            LINE_NAME,
            LINE_TYPE,
            LINE_TYPE_DESCRIPTION,
            BS_ADN,
            KSE,
            STOCKCODE)
        }
        rs.close()

        s.close()
        c.close()
        res
      case _ => ListBuffer.empty[BsTreeItem]
    }
  }
  def getHullParts(project: String): ListBuffer[HullPart] = {
    val res = ListBuffer.empty[HullPart]
    DBManager.GetOracleConnection(project) match {
      case Some(c) =>
        val s = c.createStatement()
        val query = Source.fromResource("queries/hullParts.sql").mkString
        val rs = s.executeQuery(query)
        while (rs.next()) {
          res += new HullPart(
            rs.getInt("PARTOID"),
            rs.getString("PARTNAME"),
            0,
            rs.getString("DESCRIPTION") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("BLOCKNAME"),
            rs.getDouble("WEIGHT"),
            rs.getDouble("X_COG"),
            rs.getDouble("Y_COG"),
            rs.getDouble("Z_COG"),
            "",
            rs.getInt("KSEOID")
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }


    val stdPlates = getStdPlates(project, res.filter(x => x.KSE == 0).map(x => x.PARTOID))
    val stdProfiles = getStdProfiles(project, res.filter(x => x.KSE != 0).map(x => x.KSE))


    res.foreach(part => {
      if (part.KSE != 0) {
        stdProfiles.find(x => x.KSE == part.KSE) match {
          case Some(value) =>
            part.MATERIAL = value.MATERIAL
            part.STOCK_CODE = value.STOCK_CODE
            part.SECTION = profileSectionDecode(value.SECTION) + " " + List(value.WEB_HEIGHT, value.WEB_THICKNESS, value.FLANGE_HEIGHT, value.FLANGE_THICKNESS).mkString("x")
          case _ => None
        }
      }
      else {
        stdPlates.find(x => x.PART_OID == part.PARTOID) match {
          case Some(value) =>
            part.MATERIAL = value.MATERIAL
            part.STOCK_CODE = value.STOCK_CODE
            part.SECTION = value.THICK.toString
            part.THICK = value.THICK / 1000
          case _ => None
        }
      }
    })

    res
  }
  def getHullParts(project: String, docNumber: String): ListBuffer[HullPart] = {
    val res = ListBuffer.empty[HullPart]
    DBManager.GetOracleConnection(project) match {
      case Some(c) =>
        val query = Source.fromResource("queries/hullPartsWithDescription.sql").mkString.replaceAll(":docNumber", "'" + docNumber + "'")
        val s = c.prepareStatement(query)
        val rs = s.executeQuery()
        while (rs.next()) {
          res += new HullPart(
            rs.getInt("PARTOID"),
            rs.getString("PARTNAME"),
            rs.getInt("PART_TYPE"),
            rs.getString("DESCRIPTION") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("BLOCKNAME"),
            rs.getDouble("WEIGHT"),
            rs.getDouble("X_COG"),
            rs.getDouble("Y_COG"),
            rs.getDouble("Z_COG"),
            rs.getString("BLOCK_DESCRIPTION"),
            rs.getInt("KSEOID")
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }


    val stdPlates = getStdPlates(project, res.filter(x => x.KSE == 0).map(x => x.PARTOID))
    val stdProfiles = getStdProfiles(project, res.filter(x => x.KSE != 0).map(x => x.KSE))


    res.foreach(part => {
      if (part.KSE != 0) {
        stdProfiles.find(x => x.KSE == part.KSE) match {
          case Some(value) =>
            part.MATERIAL = value.MATERIAL
            part.SECTION = profileSectionDecode(value.SECTION) + " " + List(value.WEB_HEIGHT, value.WEB_THICKNESS, value.FLANGE_HEIGHT, value.FLANGE_THICKNESS).mkString("x")
          case _ => None
        }
      }
      else {
        stdPlates.find(x => x.PART_OID == part.PARTOID) match {
          case Some(value) =>
            part.MATERIAL = value.MATERIAL
            part.SECTION = value.THICK.toString
            part.THICK = value.THICK
          case _ => None
        }
      }
    })

    res
  }
  def getStdPlates(project: String, plateOids: ListBuffer[Int]): ListBuffer[HullPartPlateDef] = {
    val res = ListBuffer.empty[HullPartPlateDef]
    plateOids.grouped(900).toList.foreach(oids => {
      DBManager.GetOracleConnection(project) match {
        case Some(c) =>
          val s = c.createStatement()
          val query = Source.fromResource("queries/hullPartsPlateDefs.sql").mkString.replaceAll("&plateOids", oids.mkString(","))
          val rs = s.executeQuery(query)
          while (rs.next()) {
            res += HullPartPlateDef(
              rs.getInt("PARTOID"),
              rs.getString("MATERIAL"),
              rs.getInt("MATERIAL_OID"),
              rs.getDouble("THICK"),
              ""
//              rs.getString("STOCK_CODE")
            )
          }
          rs.close()
          s.close()
          c.close()
        case _ =>
      }
    })

    res
  }
  def getStdProfiles(project: String, kseOids: ListBuffer[Int]): ListBuffer[HullPartProfileDef] = {
    val res = ListBuffer.empty[HullPartProfileDef]
    kseOids.grouped(900).toList.foreach(oids => {
      DBManager.GetOracleConnection(project) match {
        case Some(c) =>
          val s = c.createStatement()
          val query = Source.fromResource("queries/hullPartsProfileDefs.sql").mkString.replaceAll("&profileKse", oids.mkString(","))
          val rs = s.executeQuery(query)
          while (rs.next()) {
            res += HullPartProfileDef(
              rs.getInt("KSE"),
              rs.getInt("PROF_SECTION"),
              rs.getDouble("WEB_HEIGHT"),
              rs.getDouble("WEB_THICKNESS"),
              rs.getDouble("FLANGE_HEIGHT"),
              rs.getDouble("FLANGE_THICKNESS"),
              rs.getString("MATERIAL"),
              rs.getInt("MATERIAL_OID"),
              rs.getString("STOCK_CODE")
            )
          }
          rs.close()
          s.close()
          c.close()
        case _ =>
      }
    })

    res
  }
  def profileSectionDecode(kse: Int): String = kse match {
    case 0 =>
      "FS"
    case 1 =>
      "AS"
    case 2 =>
      "IS"
    case 3 =>
      "TS"
    case 4 =>
      "US"
    case 5 =>
      "BS"
    case 6 =>
      "ST"
    case 7 =>
      "AT"
    case 8 =>
      "OS"
    case 9 =>
      "PS"
    case 10 =>
      "RS"
    case 11 =>
      "MC"
    case 12 =>
      "DB"
    case 13 =>
      "SR"
    case 14 =>
      "HR"
    case 15 =>
      "LI"
    case 16 =>
      "ZL"
    case 17 =>
      "TL"
    case 18 =>
      "AI"
    case 19 =>
      "BL"
    case 20 =>
      "LA"
    case 21 =>
      "TA"
    case _ =>
      ""
  }
  def getSystems(project: String): ListBuffer[HullSystem] = {
    val res = ListBuffer.empty[HullSystem]
    DBManager.GetOracleConnection(project) match {
      case Some(c) =>
        val s = c.createStatement()
        val query = "SELECT NAME, DESC1 FROM V_SYSTEM"
        val rs = s.executeQuery(query)
        while (rs.next()) {
          res += HullSystem(
            rs.getString("NAME") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("DESC1") match {
              case value: String => value
              case _ => ""
            }
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def getPlates(project: String, material: String, thickness: Double): ListBuffer[PlatePart] = {
    val res = ListBuffer.empty[PlatePart]
    DBManager.GetOracleConnection(project) match {
      case Some(c) =>
        val s = c.createStatement()
        val query = Source.fromResource("queries/hullPlates.sql").mkString.replaceAll("&thickness", thickness.toString).replaceAll("&material", material)
        val rs = s.executeQuery(query)
        while (rs.next()) {
          res += PlatePart(
            rs.getString("CODE"),
            rs.getString("BLOCK") match {
              case value: String => value
              case _ => "Unknown"
            },
            rs.getString("NAME"),
            rs.getString("DESCRIPTION"),
            rs.getDouble("WEIGHT"),
            rs.getDouble("THICKNESS"),
            rs.getString("MAT"),
            rs.getString("STRGROUP") match {
              case value: String => value
              case _ => ""
            }
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }

    res
  }
  def getProfiles(project: String, material: String, kse: Int): ListBuffer[ProfilePart] = {
    val res = ListBuffer.empty[ProfilePart]
    DBManager.GetOracleConnection(project) match {
      case Some(c) =>
        val s = c.createStatement()
        val query = Source.fromResource("queries/hullProfiles.sql").mkString.replaceAll("&kse", kse.toString).replaceAll("&material", material)
        val rs = s.executeQuery(query)
        while (rs.next()) {
          res += ProfilePart(
            rs.getString("NAME"),
            rs.getString("DESCRIPTION"),
            rs.getInt("KSE"),
            rs.getString("SECTION"),
            rs.getString("MATERIAL"),
            rs.getDouble("W_H"),
            rs.getDouble("W_T"),
            rs.getDouble("F_H"),
            rs.getDouble("F_T"),
            rs.getString("BLOCK"),
            rs.getDouble("LENGTH"),
            rs.getDouble("AREA"),
            rs.getString("STOCK") match {
              case stock: String => stock
              case _ => ""
            },
            rs.getDouble("DENSITY"),
            rs.getDouble("WEIGHT"),
            rs.getString("STRGROUP") match {
              case stock: String => stock
              case _ => ""
            },
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def removeParts(project: String, block: String, parts: String, user: String): Unit = {
    DBManager.GetOracleConnection(project) match {
      case Some(c) =>
        val s = c.createStatement()
        val query = Source.fromResource("queries/removeParts.sql").mkString.replaceAll("&parts", parts).replaceAll("&block", block)
        s.execute(query)
        s.close()
        c.close()
      case _ =>
    }
    DBManager.GetOracleConnection(project) match {
      case Some(c) =>
        val s = c.createStatement()
        val date = new Date().getTime
        val query = s"insert into log_remove_parts values ('$project', '$block', '$parts', '$user', $date)"
        s.execute(query)
        s.close()
        c.close()
      case _ =>
    }
  }
  def getBsDesignNodes(project: String): ListBuffer[BsDesignNode] = {
    val res = ListBuffer.empty[BsDesignNode]
    DBManager.GetOracleConnection(project) match {
      case Some(c) =>
        val s = c.createStatement()
        val query =  Source.fromResource("queries/bsDesignNodes.sql").mkString
        val rs = s.executeQuery(query)
        while (rs.next()) {
          res += BsDesignNode(
            rs.getInt("OID") match {
              case value: Int => value
              case _ => 0
            },
            rs.getString("TYPE") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("NAME") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("DESCRIPTION") match {
              case value: String => value
              case _ => ""
            },
            rs.getInt("PARENT_NODE") match {
              case value: Int => value
              case _ => 0
            },
            rs.getInt("ATOM_TYPE") match {
              case value: Int => value
              case _ => 0
            },
            rs.getInt("BLOCK_OID") match {
              case value: Int => value
              case _ => 0
            },
            rs.getDouble("WEIGHT") match {
              case value: Double => value
              case _ => 0
            },
            rs.getDouble("X_COG") match {
              case value: Double => value
              case _ => 0
            },
            rs.getDouble("Y_COG") match {
              case value: Double => value
              case _ => 0
            },
            rs.getDouble("Z_COG") match {
              case value: Double => value
              case _ => 0
            },
            rs.getString("ATOM_NAME") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("DNA") match {
              case value: String => value
              case _ => ""
            },
            rs.getDate("MODIFY_DATE") match {
              case value: java.sql.Date  => value.getTime
              case _ => 0
            },
            rs.getString("BLOCK") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("ZONE") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("USERID") match {
              case value: String => value
              case _ => ""
            },
            rs.getInt("ELEC") match {
              case value: Int => value
              case _ => -1
            },
            rs.getInt("POS") match {
              case value: Int => value
              case _ => 0
            },
            rs.getDouble("X_MIN") match {
              case value: Double => value
              case _ => 0
            },
            rs.getDouble("Y_MIN") match {
              case value: Double => value
              case _ => 0
            },
            rs.getDouble("Z_MIN") match {
              case value: Double => value
              case _ => 0
            },
            rs.getDouble("X_MAX") match {
              case value: Double => value
              case _ => 0
            },
            rs.getDouble("Y_MAX") match {
              case value: Double => value
              case _ => 0
            },
            rs.getDouble("Z_MAX") match {
              case value: Double => value
              case _ => 0
            }
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
}
