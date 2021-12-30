import breeze.linalg.{DenseMatrix, DenseVector, Transpose}
import local.common.DBRequests
import local.domain.CommonTypes
import local.ele.CommonEle
import local.ele.cb.CableBoxManager.cableBoxBySeqId
import local.ele.eq.EleEqManager.genEqLabelsByEqOid
import local.ele.trays.{TrayHelper, TrayManager}
import local.ele.trays.TrayManager._
import org.apache.log4j.{Level, Logger}
import org.scalatest.funsuite.AnyFunSuite

class Trays  extends AnyFunSuite with TrayHelper{
  org.apache.log4j.BasicConfigurator.configure()
  Logger.getLogger( "org.mongodb.driver" ).setLevel(Level.ERROR)


  //val labs: List[String] =trayLabels("P701","18651824")//18651855

  val jj=0


/*
  val elecomplects: CommonEle.EleComplect =CommonEle.retrieveEleComplects("P701").find(s=>s.drawingId.equals("170701-884-5007")).get
*/



  //val cbx=cableBoxBySeqId("P701","18621400")

  //val zonesandsystems: List[CommonTypes.ZoneSystem] =DBRequests.retrieveZoneAndSystems("P701")

 // val cables: List[String] =TrayManager.genCablesByTraySeqId("P701","17157449")
 // val eqLabels: List[String] =genEqLabelsByEqOid("P701","9031306")
  //val traysJson: String =tarysByZonesSystemsJson("P701", List("5318"),List("884-6009"))



    //val trays=tarysByZonesSystems("P701", List("5318"),List("884-6009"))
  val trays=tarysByZonesSystems("P701", List("6306"),List("884-7002"))


/*
  val trays: List[Tray] =traysByComplect("P701", elecomplects)
*/

  //val b =genCablesInLineByTwoNodes("P701", "0000000000025851","0000000000025884")

  //trayLabels.foreach(s=>println(s))

  //val hh: (DenseMatrix[Double], DenseVector[Double]) =genCoord()
  //val ret: Transpose[DenseVector[Double]] = hh._2.t * hh._1.t

  val jj2=0
}
