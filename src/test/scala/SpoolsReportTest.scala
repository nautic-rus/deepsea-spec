
import deepsea.pipe.PipeHelper
import local.pdf.en.pipe.SpoolsReportEN.{genSpoolsListEnPDF, genSpoolsListEnPDFAll}
import org.scalatest.funsuite.AnyFunSuite


class SpoolsReportTest extends AnyFunSuite with PipeHelper {

  val ret: String = genSpoolsListEnPDF("210101-819-0001", "Air Vent and Overflow System".toUpperCase(), "0")

  //val retAll: String = genSpoolsListEnPDFAll("210101-819-0001", "FUEL SYSTEM", "0")

  println(ret)
  //println(retAll)

}
