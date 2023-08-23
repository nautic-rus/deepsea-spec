
import deepsea.esp.EspManagerHelper
import deepsea.pipe.{PipeHelper, PipeManager}
import local.pdf.en.common.ReportCommonEN.{Item11ColumnsEN, rowwrap}
import local.pdf.en.pipe.SpoolsReportEN.{genSpoolsListEnPDF, genSpoolsListEnPDFAll}
import org.davidmoten.text.utils.WordWrap
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

class SpoolsReportTest extends AnyFunSuite with PipeHelper  with EspManagerHelper{


  //val pipeSegs: List[PipeManager.PipeSeg] = getPipeSegsFromMongo("N002", "200101-574-008")
  val docNumber = "200101-574-008"
  val projectSystem = getSystemAndProjectFromDocNumber(docNumber)
  //from real db
  val pipeSegs: List[PipeManager.PipeSeg] = getPipeSegs(projectSystem._1, projectSystem._2)
  //from esp
  val esp = getPipeLatestEsp(projectSystem._1, "pipe", docNumber, "")
  val pipeSegs2 = ListBuffer.empty[PipeManager.PipeSeg]
  if (esp.nonEmpty) {
    pipeSegs2 ++= esp.get.elements
  }

  //val ret: String = genSpoolsListEnPDF("210101-800-0001", "FUEL SYSTEM", "0", jk, "ru")

  val retAll: String = genSpoolsListEnPDFAll("200101-574-008",
    "СИСТЕМА ВЕНТИЛЯЦИИ ПОМ. ПРИЕМА ПИТАНИЯ С БЕРЕГА И ПОМ. СПАСАТЕЛЬНОГО ИМУЩЕСТВАЖЖЖЖЖЖЖЖЖЖЖЖ", "0", pipeSegs2.toList, "ru")

  // println(ret)
  println(retAll)


  def generateWrappedRows(item: Item11ColumnsEN): List[Item11ColumnsEN] = {
    val A1count = 12
    val A2count = 100
    val A3count = 25
    val A4count = 20
    val A5count = 16
    val A6count = 20
    val A7count = 7
    val A8count = 10
    val A9count = 10
    val A10count = 26
    val A11count = 26
    val buff = ListBuffer.empty[Item11ColumnsEN]
    if (item.isHeader) {
      buff += item
    } else {
      val A1 = rowwrap(item.A1, A1count)
      val A2 = rowwrap(item.A2, A2count)
      val A3 = rowwrap(item.A3, A3count)
      val A4 = rowwrap(item.A4, A4count)
      val A5 = rowwrap(item.A5, A5count)
      val A6 = rowwrap(item.A6, A6count)
      val A7 = rowwrap(item.A7, A7count)
      val A8 = rowwrap(item.A8, A8count)
      val A9 = rowwrap(item.A9, A9count)
      val A10 = rowwrap(item.A10, A10count)
      val A11 = rowwrap(item.A11, A11count)

      val maxRows: Int = {
        var count = 1
        if (A1.length > count) count = A1.length
        if (A2.length > count) count = A2.length
        if (A3.length > count) count = A3.length
        if (A4.length > count) count = A4.length
        if (A5.length > count) count = A5.length
        if (A6.length > count) count = A6.length
        if (A7.length > count) count = A7.length
        if (A8.length > count) count = A8.length
        if (A9.length > count) count = A9.length
        if (A10.length > count) count = A10.length
        if (A11.length > count) count = A11.length
        count
      }
      (0 until maxRows).foreach(i => {
        buff += new Item11ColumnsEN(
          isHeader = false,
          A1.lift(i).getOrElse(""),
          A2.lift(i).getOrElse(""),
          A3.lift(i).getOrElse(""),
          A4.lift(i).getOrElse(""),
          A5.lift(i).getOrElse(""),
          A6.lift(i).getOrElse(""),
          A7.lift(i).getOrElse(""),
          A8.lift(i).getOrElse(""),
          A9.lift(i).getOrElse(""),
          A10.lift(i).getOrElse(""),
          A11.lift(i).getOrElse(""),
        )
      })
    }
    buff.toList
  }


}
