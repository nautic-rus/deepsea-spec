import local.ele.CommonEle.{EleComplectParts, retrieveAllPartsByComplectName, retrieveEleComplectsJsonString}
import local.ele.cb.CableBoxManager.cableBoxBySeqId
import local.ele.eq.EleEqManager.{genEqByOIDJson, genEqLabelsByEqOid, genEqLabelsByEqOidJson}
import local.ele.trays.TrayManager
import local.ele.trays.TrayManager.tarysByZonesSystemsJson
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer

class StressTest extends AnyFunSuite{
  val res: ListBuffer[Any] = ListBuffer.empty[Any]
  1.to(30).foreach(x => {
    res += cableBoxBySeqId("P701","18611646")
    res += retrieveEleComplectsJsonString("P701")
    res += retrieveAllPartsByComplectName("P701","170701-884-5007")
    res += genEqLabelsByEqOidJson("P701", "7119738")
    res += genEqByOIDJson("P701", "7119738")
    res += TrayManager.trayLabels("P701","18613700")
    res += cableBoxBySeqId("P701","18621400")
    res += TrayManager.genCablesByTraySeqId("P701","17157449")
    res += genEqLabelsByEqOid("P701","9031306")
    res += tarysByZonesSystemsJson("P701", List("5318"),List("884-6009"))
    val step = res(x)
  })
}
