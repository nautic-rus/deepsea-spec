import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import deepsea.database.DatabaseManager.OracleConnection
import deepsea.pipe.PipeManager.{Material, PipeSeg}
import deepsea.pipe.{PipeHelper, PipeManager}
import local.pdf.en.common.ReportCommonEN.Item11ColumnsEN
import local.pdf.en.pipe.SpoolsReportEN.genSpoolsListEnPDF
import org.mongodb.scala.MongoClient
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer

class SpoolsReportTest extends AnyFunSuite with PipeHelper {

  val docNumber = "210101-701-0001"
  val docName = "Fuel System"
  val docNumberForPrint = "210101-701-01ML"
  val systemName="701-01"

/* val docNumber = "210101-819-0001"
  val docName = "Air vent and overflow system"
  val docNumberForPrint = "210101-819-01ML"
  val systemName = "819-01"*/


  //val docNumber = "210101-702-0001"
  //val docName = "Fuel System"

  val revision = "0"
  val path = s"c:\\14\\${docNumberForPrint}_${docName}.pdf"
  val project = "N004"
  val dbData: List[PipeSeg] = {
    val configOracle = new HikariConfig()
    configOracle.setDriverClassName("oracle.jdbc.driver.OracleDriver")
    configOracle.setJdbcUrl("jdbc:oracle:thin:@office.nautic-rus.ru:1521:ORA3DB")
    configOracle.setUsername("C" + "N004")
    configOracle.setPassword("Whatab0utus")
    configOracle.setMaximumPoolSize(5)
    val ds = new HikariDataSource(configOracle)
    val mongoClient: MongoClient = MongoClient("mongodb://192.168.1.26")
    val projectSystem: (String, String) = getSystemAndProjectFromDocNumber(docNumber, mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry), ds.getConnection)
    getPipeSegsFromMongo(projectSystem._1, projectSystem._2, -1, mongoClient.getDatabase("cache").withCodecRegistry(codecRegistry), mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry))
  }

  val rawData: List[PipeSeg] = dbData.filter(s => s.spool.nonEmpty && s.system.equals(systemName))



  val rows: List[Item11ColumnsEN] = genRows(rawData)
  val totalRows: List[Item11ColumnsEN] =genTotal(rows)

  //genSpoolsListEnPDF(project, docNumber, docName, revision, path, rows)
  //genSpoolsListEnPDF(project, docNumber, docName, revision, path, rows,totalRows)
  genSpoolsListEnPDF(project, docNumberForPrint, docName, revision, path, totalRows,rows)


  private def genRows(rawData: List[PipeSeg]): List[Item11ColumnsEN] = {
    val rows: ListBuffer[Item11ColumnsEN] = ListBuffer.empty[Item11ColumnsEN]
    rawData.foreach(row => {
      val id = formatSpoolId(row.spool, row.spPieceId.toString)
      //val mat = row.material.code+ " "+ row.material.name
      val mat = row.material.name
      val qty = formatQTY(row)
      val unit = formatUnits(row.material)
      val weight: String = formatWGT(row)
      rows += Item11ColumnsEN(A1 = id, A2 = mat, A3 = unit, A4 = qty, A5 = weight,A11=row.typeCode, A12 = row.material.code)
    })
    rows.sortBy(s => s.A1).toList
  }

  private def formatSpoolId(spool: String, elem: String): String = {

    elem.length match {
      case 1 => spool + ".00" + elem
      case 2 => spool + ".0" + elem
      case _ => spool + "." + elem
    }

  }

  private def formatQTY(ps: PipeSeg): String = {

    ps.material.units match {
      case "006" => if (ps.length < 0.1) "0.1" else String.format("%.1f", ps.length)
      case "796" => {
        ps.typeCode match {
          case "PIPE" => {
            if (ps.length < 0.1) "0.1" else String.format("%.1f", ps.length)
          }
          case _ =>
            if (ps.length < 1.0)
              "1"
            else
              Math.ceil(ps.length).toInt.toString
        }
      }

      case _ => String.format("%.1f", ps.length)

    }
  }


  private def formatWGT(ps: PipeSeg): String = {
    ps.typeCode match {
      case "PIPE" => String.format("%.2f", ps.weight)
      case _ => {
        val qty = if (ps.length == 0.0) 1 else ps.length
        val w = ps.material.singleWeight * qty
        if (w < 0.01) " 0.01" else String.format("%.2f", w)
      }
    }
  }

  private def formatUnits(mat: Material): String = {
    mat.units match {
      case "006" => "m"
      case "796" => "pcs"
      case _ => "NA"
    }
  }

  private def genTotal(in: List[Item11ColumnsEN]): List[Item11ColumnsEN] = {
    val buff = ListBuffer.empty[Item11ColumnsEN]
    in.groupBy(s => s.A12).foreach(gr => {
      val ent = gr._2.head
      val qty ={
        val w=gr._2.map(_.A4.toDoubleOption.getOrElse(0.0)).sum
        if(ent.A3.equals("pcs")){
          Math.ceil(w).toInt.toString
        }else{
          if (w < 0.01) " 0.01" else String.format("%.2f", w)
        }


      }
      val wgt = {
        val w = gr._2.map(_.A5.toDoubleOption.getOrElse(0.0)).sum
        if (w < 0.01) " 0.01" else String.format("%.2f", w)
      }
      buff+=Item11ColumnsEN(A1="",A2=ent.A2,A3=ent.A3, A4=qty, A5=wgt)
    })
    buff.sortBy(s=>s.A2).toList
  }

  val hh = 0
}
