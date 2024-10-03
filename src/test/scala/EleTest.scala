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

//  val elePos1 = getElePos(foranProject, "tray", 7643449, 7431)
//  val elePos2 = getElePos(foranProject, "transit", 10173301, 7431)
//  val elePos3 = getElePos(foranProject, "equip", 5416400, 7431)
//
//  val q1 = elePos1
//  val q2 = elePos2
//  val q3 = elePos3

  val q = 0
  val ele = generateEleEsp(foranProject, docNumber, rev, user, taskId)
//  val findError = ele.elements.find(x => x.material.name.contains("ГорЦ100") && x.material.description.contains("-01"))
//  val q = findError
  val file = genElePdf(ele, "NAME OF DRAWING")
//
  println(file)
//
  Desktop.getDesktop.open(new File(file))
}
