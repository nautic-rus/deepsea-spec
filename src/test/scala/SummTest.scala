import deepsea.elec.ElePdf
import deepsea.esp.EspManager.GlobalEsp
import deepsea.esp.EspManagerHelper
import deepsea.materials.MaterialPdf
import org.scalatest.funsuite.AnyFunSuite

import java.awt.Desktop
import java.io.File
import java.nio.file.Files
import scala.collection.mutable.ListBuffer

class SummTest extends AnyFunSuite with MaterialPdf with EspManagerHelper with ElePdf{
  val foranProject = "N002"
  val docNumber = "200101-884-101"
  val rev = "0"
  val file: String = Files.createTempDirectory("hullPdf").toAbsolutePath.toString + "/" + docNumber + "_rev" + rev + ".pdf"
  val projects = List(foranProject)
  val projectId = 1
  val globalEsp = generateDeviceGlobalEsp(projects).filter(_.code == "ROMINSAUXXXX0003")
  val res = ListBuffer.empty[GlobalEsp]

  val statemId = 1
  val espMaterials = getSummaryMaterials(projectId)

  println(file)
  Desktop.getDesktop.open(new File(file))
}
