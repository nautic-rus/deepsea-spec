import local.ele.CommonEle
import local.ele.cl.CableListManager.{cablesByComplect, cablesByComplectJson, cablesByComplectMagistralVariant}
import org.scalatest.funsuite.AnyFunSuite

import scala.util.matching.Regex

class CableListTest extends AnyFunSuite {
  //val ret=cablesByComplect("P701","170701-884-5007")
  //val ret1: List[CommonEle.Cable] =cablesByComplectMagistralVariant("P701","170701-884-5007")


  val ret=cablesByComplectJson("P701","170701-884-5007")

  val hh=0

}
