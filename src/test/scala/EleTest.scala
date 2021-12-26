import local.ele.CommonEle.{retrieveAllPartsByComplectNameJSON, retrieveEleComplectsJsonString}
import local.ele.eq.EleEqManager.eqToJson
import org.scalatest.funsuite.AnyFunSuite

class EleTest extends AnyFunSuite{
  //170701-884-6009
  org.apache.log4j.BasicConfigurator.configure()
  //val b=eqsByComplect("P701","170701-884-6004")

  (1 to 100).foreach(x=>{
    println(x)
   // val complects: String =retrieveEleComplectsJsonString("P701")
    val jjj: String = retrieveAllPartsByComplectNameJSON("P701","170701-884-2001")

  })



//170701-884-2008 1
  //val jj2=retrieveAllParts("P701","170701-884-2001")

  val bb=0



}
