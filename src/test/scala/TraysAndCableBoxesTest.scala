package scala

import deepsea.elec.ElecHelper
import deepsea.elec.ElecManager.TraysAndCableBoxes
import local.pdf.ru.ele.EleTrayCableBoxReportRu.genTraysAndCBListEnPDF
import org.scalatest.funsuite.AnyFunSuite

class TraysAndCableBoxesTest extends AnyFunSuite with ElecHelper {
  val project = "N002"
  val docNumber: String = "200101-871-401"
  val traysAndCableBoxes: TraysAndCableBoxes = TraysAndCableBoxes(getTraysBySystem(project, docNumber), getCableBoxesBySystem(project, docNumber))
  val path: String = genTraysAndCBListEnPDF(docNumber, "FILL DOC NAME!", "0", traysAndCableBoxes, "ru")
  println(path)

  val a = 0;
}
