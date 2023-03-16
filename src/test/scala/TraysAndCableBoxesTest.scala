package scala

import deepsea.elec.ElecHelper
import deepsea.elec.ElecManager.TraysAndCableBoxes
import org.scalatest.funsuite.AnyFunSuite

class TraysAndCableBoxesTest extends AnyFunSuite with ElecHelper{
  val project = "N002"
  val docNumber = "200101-871-401"
  val traysAndCableBoxes = TraysAndCableBoxes(getTraysBySystem(project, docNumber), getCableBoxesBySystem(project, docNumber))
  val a = 0;
}
