import breeze.linalg.{DenseMatrix, DenseVector, Transpose}
import local.ele.eq.EleEqManager.genEqLabelsByEqOid
import local.ele.trays.TrayManager
import local.ele.trays.TrayManager._
import org.apache.log4j.{Level, Logger}
import org.scalatest.funsuite.AnyFunSuite

class Trays  extends AnyFunSuite {
  org.apache.log4j.BasicConfigurator.configure()
  Logger.getLogger( "org.mongodb.driver" ).setLevel(Level.ERROR)
  val trayLabels: List[String] =TrayManager.trayLabels("P701","17683675")

 // val cables: List[String] =TrayManager.genCablesByTraySeqId("P701","17157449")
  //val eqLabels: List[String] =genEqLabelsByEqOid("P701","9031306")

  //val a=genTraysByZoneNameAndSysName("P701", List("5318"),List("884-6009"))

  //val b =genCablesInLineByTwoNodes("P701", "0000000000025851","0000000000025884")

  //trayLabels.foreach(s=>println(s))

  //val hh: (DenseMatrix[Double], DenseVector[Double]) =genCoord()
  //val ret: Transpose[DenseVector[Double]] = hh._2.t * hh._1.t

  val jj2=0
}
