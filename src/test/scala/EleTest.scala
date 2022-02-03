import local.ele.CommonEle._
import local.ele.eq.EleEqManager.eqToJson
import org.scalatest.funsuite.AnyFunSuite

class EleTest extends AnyFunSuite{
  //170701-884-6009
  org.apache.log4j.BasicConfigurator.configure()
  //val b=eqsByComplect("P701","170701-884-6004")

  val complects: String =retrieveEleComplectsJsonString("P701")
  val jjj: EleComplectParts = retrieveAllPartsByComplectName("P701","170701-884-5007")//"170701-884-1001"






   //170701-884-2008 1
  //val jj2=retrieveAllParts("P701","170701-884-2001")

  val bb=0



}
