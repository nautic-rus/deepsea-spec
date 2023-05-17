import local.ele.CommonEle._
import local.ele.eq.EleEqManager.eqToJson
import local.pdf.ru.ele.EleTrayCableBoxReportRu.getTraysBySystem
import org.scalatest.funsuite.AnyFunSuite

class EleTest extends AnyFunSuite{
  //170701-884-6009
  org.apache.log4j.BasicConfigurator.configure()
  //val b=eqsByComplect("P701","170701-884-6004")

//  val complects: String =retrieveEleComplectsJsonString("P701")
//  val jjj: EleComplectParts = retrieveAllPartsByComplectName("P701","170701-884-5007")//"170701-884-1001"

  val qq = getTraysBySystem("N002", "200101-871-101")
  val qq1 = qq



   //170701-884-2008 1
  //val jj2=retrieveAllParts("P701","170701-884-2001")

  val bb=0



}
