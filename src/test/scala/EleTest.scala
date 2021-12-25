import local.ele.CommonEle.{retrieveAllPartsByComplectNameJSON, retrieveEleComplectsJsonString}
import local.ele.eq.EleEqManager.eqToJson
import org.scalatest.funsuite.AnyFunSuite

class EleTest extends AnyFunSuite{
  170701-884-6009

  //val b=eqsByComplect("P701","170701-884-6004")

  (0 to 100).foreach(x=>{
    val complects: String =retrieveEleComplectsJsonString("P701")

    val jjj: String = retrieveAllPartsByComplectNameJSON("P701","170701-884-2001")
    println(x)
  })



//170701-884-2008 1
  //val jj2=retrieveAllParts("P701","170701-884-2001")

  val bb=0



}
