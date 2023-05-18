
import deepsea.pipe.{PipeHelper, PipeManager}
import local.pdf.en.pipe.SpoolsReportEN.{genSpoolsListEnPDF, genSpoolsListEnPDFAll}
import org.scalatest.funsuite.AnyFunSuite


class SpoolsReportTest extends AnyFunSuite with PipeHelper {

  val pipeSegs: List[PipeManager.PipeSeg] = getPipeSegsFromMongo("N002","200101-721-004")
//  val jk = pipeSegs.filter(_.spool == "197")
//  val jkk = jk

  //val ret: String = genSpoolsListEnPDF("210101-800-0001", "FUEL SYSTEM", "0", jk, "ru")

  val retAll: String = genSpoolsListEnPDFAll("200101-721-004", "FUEL SYSTEM", "0", pipeSegs,"en")

 // println(ret)
  println(retAll)




}
