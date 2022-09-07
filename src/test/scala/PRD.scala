import local.hull.PartManager._
import local.pdf.en.common.ReportCommonEN.{DocNameEN, Item11ColumnsEN}
import local.pdf.en.prd.PrdPartsReportEN.genHullPartListEnPDF
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer

class PRD extends AnyFunSuite {

  //val s=ForanPartLabelByDrawingNumAndPartName("N004","210101-102-0203","0657")
  //val sы=genForanPartLabelByDrawingNumAndPartNameJSON("N004","NR004-101-0103","0001")
  val b=genForanPartsByDrawingNum("N002","200101-222-700")
  val h=0

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
