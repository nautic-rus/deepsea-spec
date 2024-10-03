import deepsea.hull.HullHelper
import deepsea.materials.MaterialsHelper
import local.hull.PartManager._
import local.pdf.en.common.ReportCommonEN.{DocNameEN, Item11ColumnsEN}
import local.pdf.en.prd.PrdPartsReportEN.genHullPartListEnPDF
import org.scalatest.funsuite.AnyFunSuite

import java.awt.Desktop
import java.io.File
import java.nio.file.Files
import scala.collection.mutable.ListBuffer

class PRD extends AnyFunSuite with MaterialsHelper with HullHelper {

  //val s=ForanPartLabelByDrawingNumAndPartName("N004","210101-102-0203","0657")
  //val sы=genForanPartLabelByDrawingNumAndPartNameJSON("N004","NR004-101-0103","0001")
  val project = "N002"
  val docNumber = "200101-222-BS01"
  val materials = getMaterials
  val parts = genForanPartsByDrawingNum(project, docNumber) ++ getHullIssueMaterials(docNumber, materials)
  parts.map(_.STOCK_CODE).distinct.foreach(println)
  val parts1 = parts.filter(_.STOCK_CODE == "NR00000000022398")
  val parts2 = parts.filter(_.PART_CODE == "1135")
  val qww = getHullIssueMaterials(docNumber, materials)
  val qwww = qww

  val notFound1 = parts.filter(_.PART_CODE == "0001")
  val notFound2 = parts.filter(_.PART_CODE == "0002")
  val notFound3 = parts.filter(_.PART_CODE == "0003")
  val notFound4 = parts.filter(_.PART_CODE == "0004")
  val rev = "_"
  val docName = "УСТАНОВКА НАСТИЛА В МОРОЗИЛЬНОМ ТРЮМЕ"
  val file: String = Files.createTempDirectory("hullPdf").toAbsolutePath.toString + "/" + docNumber + "_rev" + rev + ".pdf"

  genHullPartListEnPDF(project, docNumber, docName, rev, file, getHullIssueMaterials(docNumber, materials))
  Desktop.getDesktop.open(new File(file))

  val h = 0

/*  val parts: List[PrdPart] = genForanPartsByDrawingNum("N004", "210101-102-0103")
  val rows:List[Item11ColumnsEN]={
    val buff: ListBuffer[Item11ColumnsEN] =ListBuffer.empty[Item11ColumnsEN]
    parts.groupBy(s => (s.PART_CODE, s.SYMMETRY)).toList.foreach(gr => {
      val qty = gr._2.length
      val nestids: String = {
        val buff = ListBuffer.empty[String]
        gr._2.foreach(ent => buff += ent.NEST_ID)
        buff.toList.distinct.mkString(";")
      }
      val id=gr._2.head.PART_CODE
      val weight = String.format("%.2f", gr._2.head.WEIGHT_UNIT)
      val totWeight = String.format("%.2f", gr._2.head.WEIGHT_UNIT * qty)
      val symm = gr._2.head.SYMMETRY
      val elemType = gr._2.head.ELEM_TYPE
      val mat = gr._2.head.MATERIAL
      val kpl_kse = {
        elemType match {
          case "PL" =>"S"+ String.format("%.1f", gr._2.head.THICKNESS)
          case "FS" =>"S"+  String.format("%.1f", gr._2.head.THICKNESS)
          case _ => String.format("%.1f", gr._2.head.WIDTH) + "x" + String.format("%.1f", gr._2.head.THICKNESS)
        }
      }
      buff+= Item11ColumnsEN(A1=id,A2=symm,A3=elemType,A4=kpl_kse,A5=mat, A6=qty.toString,A7=weight,A8=totWeight,A9=nestids)
    })
    buff.sortBy(s=>s.A1).toList
  }
  val dn = DocNameEN(num="210101-101-0103")*/

  //genHullPartListEnPDF(dn, "c:\\1\\210101-101-0103.pdf", rows)
  val cs = 0

}
