package deepsea.hull

import akka.actor.Actor
import akka.pattern.ask
import akka.util.Timeout
import com.mongodb.BasicDBObject
import deepsea.App
import deepsea.actors.ActorManager
import deepsea.database.DatabaseManager.GetOracleConnection
import deepsea.files.FileManager.GenerateUrl
import deepsea.hull.HullManager.{GetHullEsp, GetHullEspFiles, GetHullPart, GetHullPartsByDocNumber, GetHullPartsExcel, GetHullPlatesForMaterial, GetHullProfilesForMaterial, HullEsp, HullPartPlateDef, HullPartProfileDef, PlatePart, ProfilePart, SetHullEsp}
import deepsea.hull.classes.HullPart
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import local.sql.MongoDB
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase}
import org.mongodb.scala.bson.codecs.Macros._
import org.mongodb.scala.model.Filters.equal

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
import java.sql.ResultSet
import java.util.concurrent.TimeUnit
import java.util.zip.{ZipEntry, ZipOutputStream}
import java.util.{Date, UUID}
import scala.collection.mutable.ListBuffer
import scala.io.Source


object HullManager {

  case class GetHullPartsExcel(project: String)

  case class GetHullPart(project: String, docNumber: String, partCode: String)

  case class GetHullPartsByDocNumber(project: String, docNumber: String)

  case class GetHullEsp(docNumber: String, revision: String = "")

  case class SetHullEsp(project: String, docNumber: String, user: String, revision: String)

  case class GetHullEspFiles(project: String, docNumber: String, docName: String, revision: String)

  case class HullPartPlateDef(PART_OID: Int, MATERIAL: String, MATERIAL_OID: Int, THICK: Double, STOCK_CODE: String)

  case class HullPartProfileDef(KSE: Int, SECTION: Int, WEB_HEIGHT: Double, WEB_THICKNESS: Double, FLANGE_HEIGHT: Double, FLANGE_THICKNESS: Double, MATERIAL: String, MATERIAL_OID: Int, STOCK_CODE: String)

  case class HullEsp(project: String, department: String, content: List[PrdPart], var docNumber: String, var user: String, var revision: String, var date: Long, var version: Int)

  case class GetHullPlatesForMaterial(project: String, material: String, thickness: String)
  case class GetHullProfilesForMaterial(project: String, material: String, kse: String)

  implicit val HullEspDecoder: Decoder[HullEsp] = deriveDecoder[HullEsp]
  implicit val HullEspEncoder: Encoder[HullEsp] = deriveEncoder[HullEsp]


  case class PlatePart(code: String, block: String, name: String, description: String, weight: Double, thickness: Double, material: String)
  case class ProfilePart(kse: Int, section: String, material: String, w_h: Double, w_t: Double, f_h: Double, f_t: Double, block: String, length: Double, area: Double, stock: String, density: Double, weight: Double)
}

class HullManager extends Actor {
  implicit val timeout: Timeout = Timeout(300, TimeUnit.SECONDS)

  private val espCollectionName = "hullEsp"

  private val codecRegistry: CodecRegistry = fromRegistries(fromProviders(
    classOf[HullEsp],
    classOf[PrdPart],
  ), DEFAULT_CODEC_REGISTRY)

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
      sender() ! genForanPartsByDrawingNumJSON(project, docNumber)
    case GetHullEsp(docNumber, revision) =>
      val mongo = mongoClient()
      val partLists = mongo.getDatabase(App.conf.getString("mongo.db")).getCollection(espCollectionName).withCodecRegistry(codecRegistry)
      val partListsHistory = mongo.getDatabase(App.conf.getString("mongo.db")).getCollection(espCollectionName + "History").withCodecRegistry(codecRegistry)
      val dbObject = new BasicDBObject()
      dbObject.put("docNumber", docNumber)
      if (revision != "") {
        dbObject.put("revision", revision)
      }
      Await.result(partLists.find[HullEsp](dbObject).first().toFuture(), Duration(10, SECONDS)) match {
        case existSP: HullEsp =>
          sender() ! existSP.asJson.noSpaces
        case _ =>
          Await.result(partListsHistory.find[HullEsp](dbObject).first().toFuture(), Duration(10, SECONDS)) match {
            case existSP: HullEsp =>
              sender() ! existSP.asJson.noSpaces
            case _ =>
              sender() ! "not found"
          }
      }
    case SetHullEsp(project, docNumber, user, revision) =>
      val hullParts = ForanPartsByDrawingNum(project, docNumber).sortBy(s => s.PART_CODE)
      val mongo = mongoClient()
      val partLists = mongo.getDatabase(App.conf.getString("mongo.db")).getCollection(espCollectionName).withCodecRegistry(codecRegistry)
      val partListsHistory = mongo.getDatabase(App.conf.getString("mongo.db")).getCollection(espCollectionName + "History").withCodecRegistry(codecRegistry)

      var version = 0
      Await.result(partLists.find[HullEsp](new BasicDBObject("docNumber", docNumber)).first().toFuture(), Duration(10, SECONDS)) match {
        case existSP: HullEsp =>
          version = existSP.version + 1
          Await.result(partListsHistory.insertOne(Document.apply(existSP.asJson.noSpaces)).toFuture(), Duration(10, SECONDS))
          Await.result(partLists.deleteOne(new BasicDBObject("docNumber", docNumber)).toFuture(), Duration(10, SECONDS))
        case _ => None
      }

      val date = new Date().getTime
      val hullPartList = HullEsp(project, "hull", hullParts, docNumber, user, revision, date, version)
      Await.result(partLists.insertOne(Document.apply(hullPartList.asJson.noSpaces)).toFuture(), Duration(10, SECONDS))

      sender() ! "success"
    case GetHullEspFiles(project, docNumber, docName, revision) =>
      val file: String = Files.createTempDirectory("trayPdf").toAbsolutePath.toString + "/" + docNumber + ".pdf"
      genHullPartListEnPDF(project, docNumber, docName, revision, file)
      Await.result(ActorManager.files ? GenerateUrl(file), timeout.duration) match {
        case url: String => sender() ! url.asJson.noSpaces
        case _ => sender() ! "error".asJson.noSpaces
      }

    case GetHullPlatesForMaterial(project, material, thickness) =>
      sender() ! getPlates(project, material, thickness.toDoubleOption.getOrElse(0)).asJson.noSpaces

    case GetHullProfilesForMaterial(project, material, kse) =>
      sender() ! getProfiles(project, material, kse.toIntOption.getOrElse(0)).asJson.noSpaces

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
    GetOracleConnection(project) match {
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
    GetOracleConnection(project) match {
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
          case _ => None
        }
      }
    })

    res
  }
  def getHullParts(project: String, docNumber: String): ListBuffer[HullPart] = {
    val res = ListBuffer.empty[HullPart]
    GetOracleConnection(project) match {
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
          case _ => None
        }
      }
    })

    res
  }
  def getStdPlates(project: String, plateOids: ListBuffer[Int]): ListBuffer[HullPartPlateDef] = {
    val res = ListBuffer.empty[HullPartPlateDef]
    plateOids.grouped(900).toList.foreach(oids => {
      GetOracleConnection(project) match {
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
      GetOracleConnection(project) match {
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
  def getPlates(project: String, material: String, thickness: Double): ListBuffer[PlatePart] = {
    val res = ListBuffer.empty[PlatePart]
    GetOracleConnection(project) match {
      case Some(c) =>
        val s = c.createStatement()
        val query = Source.fromResource("queries/hullPlates.sql").mkString.replaceAll("&thickness", thickness.toString).replaceAll("&material", material)
        val rs = s.executeQuery(query)
        while (rs.next()) {
          res += PlatePart(
            rs.getString("CODE"),
            rs.getString("BLOCK"),
            rs.getString("NAME"),
            rs.getString("DESCRIPTION"),
            rs.getDouble("WEIGHT"),
            rs.getDouble("THICKNESS"),
            rs.getString("MAT")
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
    GetOracleConnection(project) match {
      case Some(c) =>
        val s = c.createStatement()
        val query = Source.fromResource("queries/hullProfiles.sql").mkString.replaceAll("&kse", kse.toString).replaceAll("&material", material)
        val rs = s.executeQuery(query)
        while (rs.next()) {
          res += ProfilePart(
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
