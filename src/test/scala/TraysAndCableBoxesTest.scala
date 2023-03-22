package scala

import deepsea.elec.ElecHelper
import deepsea.elec.ElecManager.TrayAndCableBox
import local.pdf.ru.ele.EleTrayCableBoxReportRu.genTraysAndCBListEnPDF
import org.scalatest.funsuite.AnyFunSuite

class TrayAndCableBoxTest extends AnyFunSuite with ElecHelper {
  val project = "N002"
  val docNumber: String = "200101-871-401"
  val trayAndCableBox: TrayAndCableBox = TrayAndCableBox(getTraysBySystem(project, docNumber), getCableBoxesBySystem(project, docNumber))
  val path: String = genTraysAndCBListEnPDF(project, docNumber, "FILL DOC NAME!", "0", "ru")
  println(path)

  val a = 0;
}
