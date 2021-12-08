package deepsea.hull

import akka.actor.Actor
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, MediaTypes, UniversalEntity}
import akka.http.scaladsl.server.Directives.complete
import akka.stream.scaladsl.FileIO
import deepsea.App
import deepsea.database.DatabaseManager.GetOracleConnection
import deepsea.hull.HullManager.{GetForanParts, GetForanPartsExcel, HullPart, HullPartPlateDef, HullPartProfileDef}
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import play.api.libs.json.{Json, OWrites}

import java.io.{File, FileOutputStream}
import java.nio.file.Files
import java.util.{Date, UUID}
import java.util.zip.{ZipEntry, ZipOutputStream}
import scala.collection.mutable.ListBuffer
import scala.io.Source


object HullManager {
  case class GetForanParts(project: String)
  case class GetForanPartsExcel(project: String)

  case class HullPart(PARTOID: Int, PARTNAME: String, DESCRIPTION: String, BLOCKNAME: String, WEIGHT: Double, KSE: Int, var THICK: Double = 0, var MATERIAL: String = "", var SECTION: String = "")
  implicit val writesPart: OWrites[HullPart] = Json.writes[HullPart]

  case class HullPartPlateDef(PART_OID: Int, MATERIAL: String, MATERIAL_OID: Int, THICK: Double)
  case class HullPartProfileDef(KSE: Int, SECTION: Int, WEB_HEIGHT: Double, WEB_THICKNESS: Double, FLANGE_HEIGHT: Double, FLANGE_THICKNESS: Double, MATERIAL: String, MATERIAL_OID: Int)
}

class HullManager extends Actor {
  override def receive: Receive = {
    case GetForanParts(project) => Json.toJson(getForanParts(project))
    case GetForanPartsExcel(project) =>
      val parts = getForanParts(project)
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
  }

  def getForanParts(project: String): ListBuffer[HullPart] = {
    val res = ListBuffer.empty[HullPart]
    GetOracleConnection(project) match {
      case Some(c) =>
        val s = c.createStatement()
        val query = Source.fromResource("queries/hullParts.sql").mkString
        val rs = s.executeQuery(query)
        while (rs.next()) {
          res += HullPart(
            rs.getInt("PARTOID"),
            rs.getString("PARTNAME"),
            rs.getString("DESCRIPTION"),
            rs.getString("BLOCKNAME"),
            rs.getDouble("WEIGHT"),
            rs.getInt("KSEOID"),
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
  def profileSectionDecode(kse: Int) = kse match {
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
