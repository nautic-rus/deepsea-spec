import breeze.linalg.{DenseMatrix, DenseVector, Transpose}
import local.common.DBRequests
import local.common.DBRequests.retrieveAllMaterialsByProject
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


  //TODO FOR BOGDAN NEEDS CHANGE CABLETRAY LABELING. ADDED COMPLECT PARAMETER
  //val cablesByTrayAndComplect: List[String] =genCablesByTraySeqIdAndComplect("P701","17193439","170701-884-4011")
  //val cablesInLineByTwoNodesAndComplect: List[String] =genCablesInLineByTwoNodesAndComplect("P701","0000000000013605", "4404051111112087","170701-884-4011")
  //val cablesInLineByTwoNodesAndComplect: List[String] =genCablesInLineByTwoNodesAndComplect("P701","9Я19-14", "0000000000023175","170701-884-5002")


/*  val labs0 =trayLabels("P701","18658722").mkString(" , ") //лестница без крышки
  val labs1 =trayLabels("P701","18658723").mkString(" , ") //лестница с крышкой
  val labs2 =trayLabels("P701","18658724").mkString(" , ") //лоток без крышки
  val labs3 =trayLabels("P701","18658725").mkString(" , ") //лоток с крышкой
  val labs4 =trayLabels("P701","18654700").mkString(" , ")  // bez f0
  val labs5 =trayLabels("P701","18654700").mkString(" , ")  // f0


  println(labs0)
  println(labs1)
  println(labs2)
  println(labs3)
  println(labs4)
  println(labs5)
  val jj=0*/


/*
  val elecomplects: CommonEle.EleComplect =CommonEle.retrieveEleComplects("P701").find(s=>s.drawingId.equals("170701-884-5007")).get
*/



  //val cbx=cableBoxBySeqId("P701","18621400")

  //val zonesandsystems: List[CommonTypes.ZoneSystem] =DBRequests.retrieveZoneAndSystems("P701")

 // val cables: List[String] =TrayManager.genCablesByTraySeqId("P701","17157449")
 // val eqLabels: List[String] =genEqLabelsByEqOid("P701","9031306")
  //val traysJson: String =tarysByZonesSystemsJson("P701", List("5318"),List("884-6009"))

  val hh=TrayBySeqId("P701","16213583")



    //val trays=tarysByZonesSystems("P701", List("5318"),List("884-6009"))
  //val trays=tarysByZonesSystems("P701", List("6306"),List("884-7002"))


/*
  val trays: List[Tray] =traysByComplect("P701", elecomplects)
*/

  //val b =genCablesInLineByTwoNodes("P701", "0000000000025851","0000000000025884")

  //trayLabels.foreach(s=>println(s))

  //val hh: (DenseMatrix[Double], DenseVector[Double]) =genCoord()
  //val ret: Transpose[DenseVector[Double]] = hh._2.t * hh._1.t

  val jj2=0
}
