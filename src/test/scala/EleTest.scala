import deepsea.elec.{ElePdf, ElecHelper}
import local.ele.CommonEle._
import local.ele.eq.EleEqManager.eqToJson
import local.pdf.ru.ele.EleTrayCableBoxReportRu.getTraysBySystem
import org.scalatest.funsuite.AnyFunSuite

import java.awt.Desktop
import java.io.File
import java.nio.file.Files

class EleTest extends AnyFunSuite with ElecHelper with ElePdf{
  val foranProject = "N002"
  val docNumber = "200101-884-101"
  val rev = "0"
  val user = ""
  val taskId = "7431"
  val qq = 0
  val file: String = Files.createTempDirectory("hullPdf").toAbsolutePath.toString + "/" + docNumber + "_rev" + rev + ".pdf"

  val ele = generateEleEsp(foranProject, docNumber, rev, user, taskId)
  genElePdf(ele, "NAME OF DRAWING", file)

  println(file)

  Desktop.getDesktop.open(new File(file))
}
