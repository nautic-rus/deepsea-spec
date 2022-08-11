
import deepsea.pipe.{PipeHelper, PipeManager}
import local.pdf.en.pipe.SpoolsReportEN.{genSpoolsListEnPDF, genSpoolsListEnPDFAll}
import org.scalatest.funsuite.AnyFunSuite


class SpoolsReportTest extends AnyFunSuite with PipeHelper {

  val pipeSegs: List[PipeManager.PipeSeg] = getPipeSegsFromMongo("210101-819-0001")

  val ret: String = genSpoolsListEnPDF("210101-819-0001", "FUEL SYSTEM", "0", pipeSegs)

  val retAll: String = genSpoolsListEnPDFAll("210101-819-0001", "FUEL SYSTEM", "0", pipeSegs)

  println(ret)
  println(retAll)

}
