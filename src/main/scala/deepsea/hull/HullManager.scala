package deepsea.hull

import akka.actor.Actor
import com.mongodb.BasicDBObject
import deepsea.App
import deepsea.database.DatabaseManager.GetOracleConnection
import deepsea.hull.HullManager.{GetHullEsp, GetHullParts, GetHullPartsByDocNumber, GetHullPartsExcel, HullEsp, HullPartPlateDef, HullPartProfileDef, SetHullPartsByDocNumber}
import deepsea.hull.classes.HullPart
import local.hull.BStree.{BsTreeItem, PartList}
import local.sql.ConnectionManager.mongoClient
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import org.mongodb.scala.Document
import play.api.libs.json.{Json, OWrites}

import java.io.{File, FileOutputStream}
import java.nio.file.Files
import java.sql.ResultSet
import java.util.zip.{ZipEntry, ZipOutputStream}
import java.util.{Date, UUID}
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import scala.io.Source


object HullManager {
  case class GetHullParts(project: String)
  case class GetHullPartsExcel(project: String)
  case class GetHullEsp(docNumber: String)
  case class GetHullPartsByDocNumber(project: String, docNumber: String)
  case class SetHullPartsByDocNumber(project: String, docNumber: String, user: String, revision: String)

  case class HullPartPlateDef(PART_OID: Int, MATERIAL: String, MATERIAL_OID: Int, THICK: Double, STOCK_CODE: String)
  case class HullPartProfileDef(KSE: Int, SECTION: Int, WEB_HEIGHT: Double, WEB_THICKNESS: Double, FLANGE_HEIGHT: Double, FLANGE_THICKNESS: Double, MATERIAL: String, MATERIAL_OID: Int, STOCK_CODE: String)

  case class HullEsp(project: String, content: ListBuffer[HullPart], var docNumber: String, var user: String,  var revision: String, var date: Long, var version: Int)
  implicit val writesHullPartList: OWrites[HullEsp] = Json.writes[HullEsp]

}

class HullManager extends Actor {
  override def receive: Receive = {
    case GetHullParts(project) => sender() ! getHullParts(project)
    case GetHullEsp(docNumber) =>
      val mongo = mongoClient()
      val partLists = mongo.getDatabase(App.conf.getString("mongo.db")).getCollection("partLists")
      Await.result(partLists.find[HullEsp](new BasicDBObject("docNumber", docNumber)).first().toFuture(), Duration(10, SECONDS)) match {
        case existSP: HullEsp =>
          sender() ! Json.toJson(existSP)
        case _ =>
          sender() ! "not found"
      }
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
      file = new File(App.Cloud.Directory + "\\" + pathId)
      while (file.exists()){
        pathId = UUID.randomUUID().toString.substring(0, 8)
        file = new File(App.Cloud.Directory + "\\" + pathId)
      }
      file.mkdir()
      file = new File(App.Cloud.Directory + "\\" + pathId + "\\" + fileName)
      val zip = new ZipOutputStream(new FileOutputStream(file))
      Files.list(path).forEach(file => {
        zip.putNextEntry(new ZipEntry(file.getFileName.toString))
        zip.write(Files.readAllBytes(file))
      })
      zip.close()
      val fileUrl = App.Cloud.Url + "/" + pathId + "/" + fileName

      sender() ! fileUrl
    case GetHullPartsByDocNumber(project, docNumber) => sender() ! Json.toJson(getHullPartsByDocNumber(project, docNumber))

    case SetHullPartsByDocNumber(project, docNumber, user, revision) =>
      val hullParts = getHullPartsByDocNumber(project, docNumber)
      val mongo = mongoClient()
      val partLists = mongo.getDatabase(App.conf.getString("mongo.db")).getCollection("partLists")
      val partListsHistory = mongo.getDatabase(App.conf.getString("mongo.db")).getCollection("partListsHistory")

      var version = 0
      Await.result(partLists.find[HullEsp](new BasicDBObject("docNumber", docNumber)).first().toFuture(), Duration(10, SECONDS)) match {
        case existSP: HullEsp =>
          version = existSP.version + 1
          Await.result(partListsHistory.insertOne(Document.apply(Json.toJson(existSP).toString())).toFuture(), Duration(100, SECONDS))
        case _ => None
      }

      val date = new Date().getTime
      val hullPartList = HullEsp(project, hullParts, docNumber, user, revision, date, version)
      Await.result(partLists.insertOne(Document.apply(Json.toJson(hullPartList).toString())).toFuture(), Duration(100, SECONDS))

      sender() ! "success"
  }

  def getHullPartsByDocNumber(project: String, docNumber: String): ListBuffer[HullPart] ={
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
            part.STOCK_CODE = value.STOCK_CODE
            part.SECTION = profileSectionDecode(value.SECTION) + " " + List(value.WEB_HEIGHT, value.WEB_THICKNESS, value.FLANGE_HEIGHT, value.FLANGE_THICKNESS).mkString("x")
          case _ => None
        }
      }
      else{
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
      else{
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
}
