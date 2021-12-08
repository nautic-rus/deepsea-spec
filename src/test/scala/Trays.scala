import local.ele.eq.EleEqManager.genEqLabelsByEqOid
import local.ele.trays.TrayManager
import org.scalatest.funsuite.AnyFunSuite

class Trays  extends AnyFunSuite {
  org.apache.log4j.BasicConfigurator.configure()
  val trayLabels: List[String] =TrayManager.trayLabels("P701","17683679")
  val cables: List[String] =TrayManager.genCablesByTraySeqId("P701","17157449")
  val eqLabels: List[String] =genEqLabelsByEqOid("P701","9031306")
  val jj2=0
}
