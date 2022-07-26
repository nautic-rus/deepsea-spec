import deepsea.pipe.PipeHelper
import org.scalatest.funsuite.AnyFunSuite

class SpoolsReportTest extends AnyFunSuite with PipeHelper{
  val docNumber = "210101-701-0001"
  val docName = "Fuel System"
  val revision = "0"


  val projectSystem = getSystemAndProjectFromDocNumber(docNumber)
  val pipeSegs = getPipeSegs(projectSystem._1, projectSystem._2)

  val hh=0
}
